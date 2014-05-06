package com.roundeights.skene

import org.specs2.mutable._
import org.specs2.mock._

import scala.concurrent._
import scala.concurrent.duration._
import java.util.concurrent.Executor

class GraphTest extends Specification with Mockito {

    /** An execution context that runs in the calling thread */
    implicit val context = ExecutionContext.fromExecutor(new Executor {
        override def execute( command: Runnable ): Unit = command.run
    })

    /** Blocks and returns the result of a future */
    def await[T] ( future: Future[T] ): T
        = Await.result( future, Duration(10, "second") )

    val req = mock[Request]
    val resp = mock[Response]
    val recover = mock[Recover]

    trait Req1 { def one: String }
    trait Req2 { def two: String }
    trait Req3 { def three: String }

    "A list of prereqs" should {

        "return a processed response" in {
            val registry = new Registry()
                .register[Req1](
                    (result: Promise[Req1]) => {
                        result.success(new Req1{ val one = "1" })
                    }
                )
                .register[Req2](
                    (result: Promise[Req2]) => {
                        result.success(new Req2{ val two = "2" })
                    }
                )

            val bundle = await( registry.use[Req1, Req2].build(req, resp) )

            bundle.request must_== req
            bundle.response must_== resp
            bundle.one must_== "1"
            bundle.two must_== "2"
        }

        "be able to access their dependencies" in {

            val registry = new Registry()
                .register[Req1](
                    (bundle: Bundle, result: Promise[Req1]) => {
                        bundle.get[Req2].two must_== "2"
                        bundle.get[Req3].three must_== "3"
                        result.success( new Req1{ val one = "1" } )
                    },
                    classOf[Req2],
                    classOf[Req3]
                )
                .register[Req2](
                    (result: Promise[Req2]) => {
                        result.success(new Req2{ val two = "2" })
                    }
                )
                .register[Req3](
                    (result: Promise[Req3]) => {
                        result.success(new Req3{ val three = "3" })
                    }
                )

            await( registry.use[Req1, Req2, Req3].build(req, resp) )

            ok
        }

        "fail the bundle future if they fail their req promise" in {
            val err = new Exception("Expected")

            val registry = new Registry()
                .register[Req1](
                    (result: Promise[Req1]) => result.failure( err )
                )

            await( registry.use[Req1].build(req, resp).failed ) must_== err
        }

        "fail the bundle future if they throw" in {
            val err = new Exception("Expected")

            val registry = new Registry()
                .register[Req1]( (result: Promise[Req1]) => throw err )

            await( registry.use[Req1].build(req, resp).failed ) must_== err
        }

    }

    "A Registry used in for comprehensions" should {

        val registry = new Registry()
            .register[Req1](
                (bundle: Bundle, result: Promise[Req1]) => {
                    result.success( new Req1{ val one = "1" } )
                }
            )

        "support a simple foreach" in {

            val promise = Promise[String]()

            var handler = for {
                (reqs, _, _) <- registry.use[Req1]
            } promise.success( reqs.one )

            handler.handle( recover, req, resp )
            await( promise.future ) must_== "1"
        }

        "Allow assignments" in {

            val promise = Promise[String]()

            var handler = for {
                (reqs, _, _) <- registry.use[Req1]
                delim = ": "
                label = "Number"
            } promise.success( label + delim + reqs.one )

            handler.handle( recover, req, resp )
            await( promise.future ) must_== "Number: 1"
        }

    }

}


