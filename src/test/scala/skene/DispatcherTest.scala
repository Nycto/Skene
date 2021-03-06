package test.scala.com.skene

import org.specs2.mutable._
import org.specs2.mock._

import com.roundeights.skene._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._

class DispatcherTest extends Specification with Mockito {

    // An error recovery mechanism that just rethrows
    val recover = Recover.using { case err: Throwable => throw err }

    // A shared request object between the tests
    val request = BareRequest()

    // The renderable object that is returned when a test passes
    val response = mock[Response]

    // A handler that fails when it is invoked
    val uncallableHandler = {
        val handler = mock[Handler]
        handler.handle(recover, request, response) throws new RuntimeException(
            "The wrong Handler was called"
        )
        handler
    }

    "A Dispatcher" should {

        "match in the order that add is called" in {
            val handler = mock[Handler]

            new Dispatcher()
                .add( Matcher.always, handler )
                .add( Matcher.always, uncallableHandler )
                .handle( recover, request, response )

            there was one(handler).handle( recover, request, response )
        }

        "not call handlers when the matcher doesn't pass" in {
            val handler = mock[Handler]

            new Dispatcher()
                .add( Matcher.never, uncallableHandler )
                .add( Matcher.never, uncallableHandler )
                .add( Matcher.always, handler )
                .handle( recover, request, response )

            there was one(handler).handle( recover, request, response )
        }

        "set parameters when a matcher returns them" in {

            val matcher = Matcher.call {
                Matcher.Result(true, Map("1" -> "a"))
            }

            val handler = Handler( (req, resp) => {
                req.params must_== Map("1" -> "a")
            } )

            new Dispatcher()
                .add( matcher, handler )
                .handle( recover, request, response )

            ok
        }
    }

    "The default handler in a Dispatcher" should {

        "Be called when none of the other matchers take" in {
            val handler = mock[Handler]

            new Dispatcher()
                .add( Matcher.never, uncallableHandler )
                .add( Matcher.never, uncallableHandler )
                .default( handler )
                .handle( recover, request, response )

            there was one(handler).handle( recover, request, response )
        }
    }

    "The error handler of a Dispatcher" should {

        val err = new ClassNotFoundException("Should be caught by test")

        val throwingHandler = new Handler {
            override def handle(
                recover: Recover, request: Request, response: Response
            ): Unit = throw err
        }

        "be called when an exception is thrown" in {
            val runnable = mock[Runnable]

            new Dispatcher()
                .add( Matcher.always, throwingHandler )
                .error ( (req, resp) => {
                    case thrown: Throwable => {
                        thrown must_== err
                        req must_== request
                        resp must_== response
                        runnable.run
                    }
                })
                .handle( recover, request, response )

            there was one(runnable).run()
        }

        "be called when the default handler throws an exception" in {
            val runnable = mock[Runnable]

            new Dispatcher()
                .default( throwingHandler )
                .error ( (req, resp) => {
                    case thrown: Throwable => {
                        thrown must_== err
                        req must_== request
                        resp must_== response
                        runnable.run
                    }
                })
                .handle( recover, request, response )

            there was one(runnable).run()
        }

        "Call the default recovery instance when none is defined" in {
            val runnable = mock[Runnable]

            val recover = Recover.using {
                case thrown: Throwable => {
                    thrown must_== err
                    runnable.run()
                }
            }

            new Dispatcher()
                .add( Matcher.always, throwingHandler )
                .handle( recover, request, response )

            there was one(runnable).run()
        }

        "Call the default recovery instance when the error handler " +
        "is not defined for the given throwable" in {
            val runnable = mock[Runnable]

            val recover = Recover.using {
                case thrown: Throwable => {
                    thrown must_== err
                    runnable.run()
                }
            }

            new Dispatcher()
                .add( Matcher.always, throwingHandler )
                .error ( (req, resp) => {
                    case thrown: NullPointerException => ()
                })
                .handle( recover, request, response )

            there was one(runnable).run()
        }

        "Call the default recovery instance when the error handler throws" in {
            val runnable = mock[Runnable]

            val recover = Recover.using {
                case thrown: Throwable => {
                    thrown must_== err
                    runnable.run()
                }
            }

            new Dispatcher()
                .add( Matcher.always, throwingHandler )
                .error ( (req, resp) => {
                    case thrown: Throwable => throw thrown
                })
                .handle( recover, request, response )

            there was one(runnable).run()
        }

        "Be given a reference to the default recover instance" in {
            val runnable = mock[Runnable]

            val recover = Recover.using {
                case thrown: Throwable => {
                    thrown must_== err
                    runnable.run()
                }
            }

            new Dispatcher()
                .add( Matcher.always, throwingHandler )
                .error ( (rec, req, resp) => {
                    case thrown: Throwable => rec.orRethrow(thrown)
                })
                .handle( recover, request, response )

            there was one(runnable).run()
        }

    }

    "A Dispatcher used as a Matcher" should {

        "Not match when it is empty" in {
            new Dispatcher().matches( request ) must_== Matcher.Result(false)
        }

        "Match when at least one of its handlers matches" in {
            new Dispatcher()
                .add( Matcher.never, mock[Handler] )
                .add( Matcher.always, mock[Handler] )
                .add( Matcher.never, mock[Handler] )
                .matches( request ) must_== Matcher.Result(true)
        }

        "Match when a default handler exists" in {
            new Dispatcher().default( mock[Handler] )
                .matches( request ) must_== Matcher.Result(true)
        }

        "Not match when none of its handlers matches" in {
            new Dispatcher()
                .add( Matcher.never, mock[Handler] )
                .add( Matcher.never, mock[Handler] )
                .matches( request ) must_== Matcher.Result(false)
        }
    }

    "Observers in a dispatcher" should {

        "Execute when they match" in {
            var one = Promise[Unit]()
            var two = Promise[Unit]()
            new Dispatcher()
                .observe(Matcher.always, _ => one.success(Unit))
                .observe(Matcher.always, _ => two.success(Unit))
                .add( Matcher.always, mock[Handler] )
                .handle( recover, request, response )

            Await.result( one.future, Duration(3, "second") )
            Await.result( two.future, Duration(3, "second") )
            ok
        }

        "Not execute when don't match" in {
            new Dispatcher()(new ExecutionContext {
                override def execute(r: Runnable) = r.run
                override def reportFailure(e: Throwable) = ()
            })
                .observe(Matcher.never, _ => throw new RuntimeException )
                .observe(Matcher.never, _ => throw new RuntimeException )
                .add( Matcher.always, mock[Handler] )
                .handle( recover, request, response )

            ok
        }
    }

}

