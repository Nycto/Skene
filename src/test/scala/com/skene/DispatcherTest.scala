package test.scala.com.skene

import org.specs2.mutable._
import org.specs2.mock._

import main.scala.com.skene._

class DispatcherTest extends Specification with Mockito {

    // A shared context object between the tests
    val context = mock[Context]

    // The renderable object that is returned when a test passes
    val rendered = Renderable("Pass")

    // A handler that allows itself to be called
    val handlerCallable = {
        val handler = mock[Handler]
        handler.handle(context) returns rendered
        handler
    }

    // A handler that fails when it is invoked
    val handlerUncallable = {
        val handler = mock[Handler]
        handler.handle(context) throws new RuntimeException(
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

            dispatcher.handle( context ) must_== rendered
        }

        "not call handlers when the matcher doesn't pass" in {
            val dispatcher = {
                (new Dispatcher)
                    .add( Matcher.never, handlerUncallable )
                    .add( Matcher.never, handlerUncallable )
                    .add( Matcher.always, handlerCallable )
            }

            dispatcher.handle( context ) must_== rendered
        }
    }

}

