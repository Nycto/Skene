package test.scala.com.skene

import org.specs2.mutable._
import org.specs2.mock._

import org.skene._

class DispatcherTest extends Specification with Mockito {

    // A shared request object between the tests
    val request = BareRequest()

    // The renderable object that is returned when a test passes
    val response = Response("Pass")

    // A handler that allows itself to be called
    val handlerCallable = {
        val handler = mock[Handler]
        handler.handle( any ) returns response
        handler
    }

    // A handler that fails when it is invoked
    val handlerUncallable = {
        val handler = mock[Handler]
        handler.handle(request) throws new RuntimeException(
            "The wrong Handler was called"
        )
        handler
    }

    "A Dispatcher" should {
        "match in the order that add is called" in {
            val dispatcher = {
                (new Dispatcher)
                    .add( Matcher.always, handlerCallable )
                    .add( Matcher.always, handlerUncallable )
            }

            dispatcher.handle( request ) must_== response
        }

        "not call handlers when the matcher doesn't pass" in {
            val dispatcher = {
                (new Dispatcher)
                    .add( Matcher.never, handlerUncallable )
                    .add( Matcher.never, handlerUncallable )
                    .add( Matcher.always, handlerCallable )
            }

            dispatcher.handle( request ) must_== response
        }

        "set parameters when a matcher returns them" in {

            val matcher = Matcher.call {
                Matcher.Result(true, Map("1" -> "a"))
            }

            val handler = Handler( request => {
                request.params must_== Map("1" -> "a")
                response
            } )

            val dispatcher = (new Dispatcher).add( matcher, handler )

            dispatcher.handle( request )

            success
        }
    }

    "The default handler in a Dispatcher" should {
        "Be called when none of the other matchers take" in {
            val dispatcher = {
                (new Dispatcher)
                    .add( Matcher.never, handlerUncallable )
                    .add( Matcher.never, handlerUncallable )
            }

            dispatcher.default( handlerCallable )

            dispatcher.handle( request ) must_== response
        }
    }

    "The error handler of a Dispatcher" should {

        val throwingHandler = Handler( (request) => {
            throw new Exception
        })

        "be called when an exception is thrown" in {
            val dispatcher =
                new Dispatcher( Logger.nil )
                .add( Matcher.always, throwingHandler )

            dispatcher.error( (err, request) => response )

            dispatcher.handle( request ) must_== response
        }

        "bw called when the default handler throws an exception" in {
            val dispatcher =
                new Dispatcher( Logger.nil )
                .add( Matcher.always, throwingHandler )

            dispatcher.error( (err, request) => response )

            dispatcher.handle( request ) must_== response
        }

    }

}

