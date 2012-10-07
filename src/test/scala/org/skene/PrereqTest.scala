package test.scala.com.skene

import org.specs2.mutable._
import org.specs2.mock._

import org.skene._

class PrereqTest extends Specification with Mockito {

    val request = BareRequest()
    val response = mock[Response]

    trait Req1 { def one: String }
    trait Req2 { def two: String }
    trait Req3 { def three: String }

    "A request with an unregistered prereq" should {
        "raise an exception" in {
            val prereqs = Registry()
            prereqs[Req1] ( (prereq, resp) => () ) must throwA[
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
                        bundle: Bundle, next: Continue[Req1]
                    ) = ()
                })

            prereqs[Req1] ( (prereq, resp) => () ) must throwA[
                Registry.UnregisteredPrereq
            ]
        }
    }

    "Two prereqs with conflicting interfaces" should {

        "raise an exception" in {

            trait Req3Conflict { def three: String }

            val prereqs = Registry()
                .register[Req3](
                    (next: Continue[Req3])
                        => next(new Req3 { val three = "3" })
                )
                .register[Req3Conflict](
                    (next: Continue[Req3Conflict])
                        => next(new Req3Conflict { val three = "Troix" })
                )

            prereqs[Req3, Req3Conflict] ( (req, resp) => () ) must throwA[
                Registry.ConflictingPrereqs
            ]
        }

        "ignore toString methods" in {
            trait Req4 { override def toString = "Req4" }
            trait Req5 { override def toString = "Req5" }

            val prereqs = Registry()
                .register[Req4]( (next: Continue[Req4]) => next(new Req4 {}) )
                .register[Req5]( (next: Continue[Req5]) => next(new Req5 {}) )

            prereqs[Req4, Req5] ( (req, resp) => () )

            ok
        }
    }

    "The dependencies of a class" should {
        val prereqs = Registry()
            .register[Req1](
                (next: Continue[Req1]) => { },
                classOf[Req2], classOf[Req3]
            )
            .register[Req2](
                (next: Continue[Req2]) => { },
                classOf[Req3]
            )
            .register[Req3]( (next: Continue[Req3]) => { } )

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

    // An alternate threading method that allows the unit tests to be run
    // without multiple threads
    def synchronized ( thunk: => Unit ): Unit = thunk

    "A request with a succesfully processed prereq" should {
        "return a processed response" in {
            val prereqs = new Registry( synchronized )
                .register[Req1](
                    (next: Continue[Req1]) => next(new Req1{ val one = "1" })
                )
                .register[Req2](
                    (next: Continue[Req2]) => next(new Req2{ val two = "2" })
                )
                .register[Req3](
                    (next: Continue[Req3]) => next(new Req3{ val three = "3" })
                )

            val handler = prereqs[Req1, Req2, Req3]( (bundle, resp) => {
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

            val prereqs = new Registry( synchronized )
                .register[Req1](
                    (bundle: Bundle, next: Continue[Req1]) => {
                        bundle.get[Req2].two must_== "2"
                        bundle.get[Req3].three must_== "3"
                        next( new Req1{ val one = "1" } )
                    },
                    classOf[Req2],
                    classOf[Req3]
                )
                .register[Req2](
                    (next: Continue[Req2]) => next(new Req2{ val two = "2" })
                )
                .register[Req3](
                    (next: Continue[Req3]) => next(new Req3{ val three = "3" })
                )

            val handler = prereqs[Req1, Req2, Req3]( (bundle, resp) => {
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

        val prereqs = new Registry( synchronized )
            .register[Req1]( (bundle: Bundle, next: Continue[Req1]) => {} )
            .register[Req2](
                (bundle: Bundle, next: Continue[Req2])
                    => throw new Exception("Should not be called"),
                classOf[Req1]
            )

        "return the prereq response" in {
            val handler = prereqs[Req1]( (bundle, resp) => {
                throw new Exception("Callback should not be invoked")
            } )
            ok
        }

        "return the response from a failing dependency" in {
            val handler = prereqs[Req2]( (bundle, resp) => {
                throw new Exception("Callback should not be invoked")
            } )
            ok
        }

    }

}


