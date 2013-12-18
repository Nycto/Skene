package test.scala.com.skene

import org.specs2.mutable._
import org.specs2.mock._

import scala.concurrent._
import scala.concurrent.duration._
import java.util.concurrent.Executor
import com.roundeights.skene._

class RecoverTest extends Specification with Mockito {

    /** Blocks while waiting for the given future */
    def await[T] ( future: Future[T] ): T
        = Await.result( future, Duration(10, "second") )

    /** An execution context that runs in the calling thread */
    implicit val context = ExecutionContext.fromExecutor(new Executor {
        override def execute( command: Runnable ): Unit = command.run
    })

    val err = new ClassNotFoundException("Should be caught by exception")

    // Returns a mock recover instance that expects 'err' and calls a runnable
    def mockRecover = {
        val runnable = mock[Runnable]

        val recover = Recover.using {
            case thrown: Throwable => {
                thrown must_== err
                runnable.run
            }
        }

        (runnable, recover)
    }

    // Asserts that a future should fail with the 'err' exception
    def expectErr[O] ( onSuccess: Recover#OnSuccess[O] ): Unit = {
        try {
            await( onSuccess.future )
            throw new Exception("An expected exception was not thrown")
        } catch {
            case thrown: Throwable if thrown == err => ()
        }
    }

    "A Recover instance" should {

        "Absorb an exception if it can" in {
            val (runnable, recover) = mockRecover

            recover.from { throw err }
            recover.orRethrow( err )

            there were two(runnable).run
        }

        "Rethrow if an exception can't be handled" in {
            val recover = Recover.using {
                case thrown: NoSuchMethodException => ()
            }

            recover.from { throw err } must throwA[ClassNotFoundException]
            recover.orRethrow( err ) must throwA[ClassNotFoundException]
        }

        "Use a fallback if defined" in {
            val runnable = mock[Runnable]

            val recover = Recover.using {
                case thrown: NoSuchMethodException => ()
            } orFallBackTo { Recover.using {
                case thrown: Throwable => {
                    thrown must_== err
                    runnable.run
                }
            }}

            recover.from { throw err }

            there was one(runnable).run
        }

        "Use a fallback if the handler throws" in {
            val runnable = mock[Runnable]

            val recover = Recover.using {
                case thrown: Throwable => throw err
            } orFallBackTo { Recover.using {
                case thrown: Throwable => {
                    thrown must_== err
                    runnable.run
                }
            }}

            recover.from { throw new Exception }

            there was one(runnable).run
        }

        "Ignore a fallback if not needed" in {
            val runnable = mock[Runnable]

            val recover = Recover.using {
                case thrown: Throwable => {
                    thrown must_== err
                    runnable.run
                }
            } orFallBackTo { Recover.using {
                case thrown: Throwable
                    => throw new Exception("Should not be called")
            }}

            recover.from { throw err }

            there was one(runnable).run
        }

        "Respond to failed futures" in {
            val (runnable, recover) = mockRecover

            expectErr {
                recover.fromFuture( Future.failed(err) )
            }

            there was one(runnable).run
        }

        "Recover when a chained 'onSuccess' function throws" in {
            val (runnable, recover) = mockRecover

            expectErr {
                recover.fromFuture(
                    Future.successful("Success")
                ).onSuccess {
                    case "Success" => throw err
                }
            }

            there was one(runnable).run
        }

        "Recover when a chained 'map' throws" in {
            val (runnable, recover) = mockRecover

            expectErr {
                recover.fromFuture(
                    Future.successful("Success")
                ).map(value => {
                    value must_== "Success"
                    throw err
                })
            }

            there was one(runnable).run
        }

        "Recover when a chained 'flatMap' throws" in {
            val (runnable, recover) = mockRecover

            expectErr {
                recover.fromFuture(
                    Future.successful("Success")
                ).flatMap(value => {
                    value must_== "Success"
                    throw err
                })
            }

            there was one(runnable).run
        }

        "Recover when a chained 'flatMap' returns a failed Future" in {
            val (runnable, recover) = mockRecover

            expectErr {
                recover.fromFuture(
                    Future.successful("Success")
                ).flatMap(value => {
                    value must_== "Success"
                    Future.failed( err )
                })
            }

            there was one(runnable).run
        }

        "recover when a chained 'foreach' throws" in {
            val (runnable, recover) = mockRecover

            val onSuccess = recover.fromFuture(
                Future.successful("success")
            )

            onSuccess.foreach(value => {
                value must_== "success"
                throw err
            })

            await( onSuccess.future )

            there was one(runnable).run
        }

        "recover when a chained 'withFilter' throws" in {
            val (runnable, recover) = mockRecover

            expectErr {
                recover.fromFuture(
                    Future.successful("Success")
                ).withFilter(value => {
                    value must_== "Success"
                    throw err
                })
            }

            there was one(runnable).run
        }

        "Not recover when a chained 'withFilter' returns false" in {
            val runnable = mock[Runnable]
            val recover = Recover.using { case _: Throwable => runnable.run }

            await(
                recover.fromFuture(
                    Future.successful("Success")
                ).withFilter(value => {
                    value must_== "Success"
                    false
                }).future.failed
            )

            there was no(runnable).run
        }

        "Provide a PartialFunction that will match all throwables" in {
            val (runnable, recover) = mockRecover
            recover.matchAll( err )
            there was one(runnable).run
        }

        "Allow a PartialFunction to be built that will handle exceptions" in {
            val (runnable, recover) = mockRecover

            val matcher = recover.matcher {
                case _: IllegalArgumentException => err
            }

            matcher( new IllegalArgumentException )
            there was one(runnable).run
        }

        "Pass through the error when a PartialFunction doesn't match" in {
            val (runnable, recover) = mockRecover

            val matcher = recover.matcher {
                case _: IllegalArgumentException => err
            }

            matcher( err )
            there was one(runnable).run
        }

        "allow 'using' to define an easy fallback" in {
            val (runnable, recover) = mockRecover

            recover.using {
                case thrown: ClassNotFoundException => thrown must_== err
            } from {
                throw err
            }

            there was no(runnable).run
        }

        "Fall back from 'using' when the error doesn't match" in {
            val (runnable, recover) = mockRecover

            recover.using {
                case thrown: IllegalArgumentException => ()
            } from {
                throw err
            }

            there was one(runnable).run
        }

    }

}

