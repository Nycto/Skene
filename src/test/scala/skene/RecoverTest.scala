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

    "A Recover instance" should {

        "Absorb an exception if it can" in {
            val runnable = mock[Runnable]

            val recover = Recover.using {
                case thrown: Throwable => {
                    thrown must_== err
                    runnable.run
                }
            }

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
            val runnable = mock[Runnable]

            val recover = Recover.using {
                case thrown: Throwable => {
                    thrown must_== err
                    runnable.run
                }
            }

            val future = Future.failed(err)

            recover.fromFuture( future )

            try {
                await( future )
            } catch {
                case err: Throwable => ()
            }

            there was one(runnable).run
       }

    }

}

