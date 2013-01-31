package test.scala.com.skene

import org.specs2.mutable._
import org.specs2.mock._

import com.roundeights.skene._
import scala.concurrent.{ExecutionContext, Promise}
import java.util.concurrent.Executor

class PrereqTest extends Specification with Mockito {

    /** An execution context that runs in the calling thread */
    implicit val context = ExecutionContext.fromExecutor(new Executor {
        override def execute( command: Runnable ): Unit = command.run
    })


    val request = BareRequest()
    val response = mock[Response]

    trait Req1 { def one: String }
    trait Req2 { def two: String }
    trait Req3 { def three: String }

    "A request with an unregistered prereq" should {
        "raise an exception" in {
            val prereqs = Registry()
            prereqs.use[Req1].in( (prereq, resp) => () ) must throwA[
                Registry.UnregisteredPrereq
            ]
        }
    }

    "An unregistered dependency" should {
        "raise an exception" in {
            val prereqs = Registry()
                .register[Req1](new Provider[Req1] {
                    override def dependencies = Set(classOf[Req2])
                    override def build(
                        bundle: Bundle, result: Promise[Req1]
                    ) = ()
                })

            prereqs.use[Req1].in( (_, _) => () ) must throwA[
                Registry.UnregisteredPrereq
            ]
        }
    }

    "Two prereqs with conflicting interfaces" should {

        "raise an exception" in {

            trait Req3Conflict { def three: String }

            val prereqs = Registry()
                .register[Req3](
                    (result: Promise[Req3]) => {
                        result.success( new Req3 { val three = "3" } )
                    }
                )
                .register[Req3Conflict](
                    (result: Promise[Req3Conflict]) => {
                        result.success( new Req3Conflict { val three = "Troix" } )
                    }
                )

            prereqs.use[Req3, Req3Conflict].in( (_, _) => () ) must throwA[
                Registry.ConflictingPrereqs
            ]
        }

        "ignore toString methods" in {
            trait Req4 { override def toString = "Req4" }
            trait Req5 { override def toString = "Req5" }

            val prereqs = Registry()
                .register[Req4](
                    (result: Promise[Req4]) => {
                        result.success(new Req4 {})
                    }
                )
                .register[Req5](
                    (result: Promise[Req5]) => {
                        result.success(new Req5 {})
                    }
                )

            prereqs.use[Req4, Req5].in( (req, resp) => () )

            ok
        }
    }

    "The dependencies of a class" should {
        val prereqs = Registry()
            .register[Req1](
                (result: Promise[Req1]) => { },
                classOf[Req2], classOf[Req3]
            )
            .register[Req2](
                (result: Promise[Req2]) => { },
                classOf[Req3]
            )
            .register[Req3]( (result: Promise[Req3]) => { } )

        "be listable" in {
            prereqs.dependenciesOf( classOf[Req1] ) must_== List(
                classOf[Req3], classOf[Req2], classOf[Req1]
            )

            prereqs.dependenciesOf( classOf[Req2] ) must_== List(
                classOf[Req3], classOf[Req2]
            )

            prereqs.dependenciesOf( classOf[Req3] ) must_== List(classOf[Req3])
        }
    }

    "A request with a succesfully processed prereq" should {
        "return a processed response" in {
            val prereqs = new Registry()
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
                .register[Req3](
                    (result: Promise[Req3]) => {
                        result.success(new Req3{ val three = "3" })
                    }
                )

            val handler = prereqs.use[Req1, Req2, Req3].in( (bundle, resp) => {
                resp must_== response
                bundle.request must_== request
                bundle.one must_== "1"
                bundle.two must_== "2"
                bundle.three must_== "3"
            } )

            handler.handle( request, response )
            ok
        }
    }

    "A prereq with dependencies" should {
        "be able to access those dependencies" in {

            val prereqs = new Registry()
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

            val handler = prereqs.use[Req1, Req2, Req3].in( (bundle, resp) => {
                resp must_== response
                bundle.request must_== request
                bundle.one must_== "1"
                bundle.two must_== "2"
                bundle.three must_== "3"
            } )

            handler.handle( request, response )
            ok
        }
    }

    "A request with a failing prereq" should {

        val prereqs = new Registry()
            .register[Req1]( (bundle: Bundle, result: Promise[Req1]) => () )
            .register[Req2](
                (bundle: Bundle, result: Promise[Req2])
                    => throw new Exception("Should not be called"),
                classOf[Req1]
            )

        "return the prereq response" in {
            prereqs.use[Req1].in( (bundle, resp) => {
                throw new Exception("Callback should not be invoked")
            } )
            ok
        }

        "return the response from a failing dependency" in {
            prereqs.use[Req2].in( (bundle, resp) => {
                throw new Exception("Callback should not be invoked")
            } )
            ok
        }

    }

}


