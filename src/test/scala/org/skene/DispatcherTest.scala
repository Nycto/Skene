package test.scala.com.skene


import org.specs2.mutable._
import org.specs2.mock._

import org.skene._

class DispatcherTest extends Specification with Mockito {

    // A shared request object between the tests
    val request = BareRequest()

    // The renderable object that is returned when a test passes
    val response = mock[Response]

    // A handler that fails when it is invoked
    val uncallableHandler = {
        val handler = mock[Handler]
        handler.handle(request, response) throws new RuntimeException(
            "The wrong Handler was called"
        )
        handler
    }

    "A Dispatcher" should {

        "match in the order that add is called" in {
            val handler = mock[Handler]

            new Dispatcher( Logger.nil )
                .add( Matcher.always, handler )
                .add( Matcher.always, uncallableHandler )
                .handle( request, response )

            there was one(handler).handle( request, response )
        }

        "not call handlers when the matcher doesn't pass" in {
            val handler = mock[Handler]

            new Dispatcher( Logger.nil )
                .add( Matcher.never, uncallableHandler )
                .add( Matcher.never, uncallableHandler )
                .add( Matcher.always, handler )
                .handle( request, response )

            there was one(handler).handle( request, response )
        }

        "set parameters when a matcher returns them" in {

            val matcher = Matcher.call {
                Matcher.Result(true, Map("1" -> "a"))
            }

            val handler = Handler( (req, resp) => {
                req.params must_== Map("1" -> "a")
            } )

            new Dispatcher( Logger.nil )
                .add( matcher, handler )
                .handle( request, response )

            ok
        }
    }

    "The default handler in a Dispatcher" should {

        "Be called when none of the other matchers take" in {
            val handler = mock[Handler]

            new Dispatcher( Logger.nil )
                .add( Matcher.never, uncallableHandler )
                .add( Matcher.never, uncallableHandler )
                .default( handler )
                .handle( request, response )

            there was one(handler).handle( request, response )
        }
    }

    "The error handler of a Dispatcher" should {

        val err = new Exception
        val throwingHandler = new Handler {
            override def handle( request: Request, response: Response ): Unit
                = throw err
        }

        "be called when an exception is thrown" in {
            val callback = mock[Function3[Throwable, Request, Response, Unit]]

            new Dispatcher( Logger.nil )
                .add( Matcher.always, throwingHandler )
                .error( callback )
                .handle( request, response )

            there was one(callback).apply( err, request, response )
        }

        "bw called when the default handler throws an exception" in {
            val callback = mock[Function3[Throwable, Request, Response, Unit]]

            new Dispatcher( Logger.nil )
                .default( throwingHandler )
                .error( callback )
                .handle( request, response )

            there was one(callback).apply( err, request, response )
        }
    }

}

