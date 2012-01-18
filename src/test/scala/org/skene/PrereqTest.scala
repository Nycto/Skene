package test.scala.com.skene

import org.specs2.mutable._

import org.skene._

class PrereqTest extends Specification {

    val request = BareRequest()
    val response = Response()

    trait Req1 { def one: String }
    trait Req2 { def two: String }
    trait Req3 { def three: String }

    "A request with an unregistered prereq" should {
        "raise an exception" in {
            val prereqs = Registry()
            prereqs[Req1] ( (prereq) => Response() ) must throwA[
                Registry.UnregisteredPrereq
            ]
        }
    }

    "Two prereqs with conflicting interfaces" should {

        "raise an exception" in {
            trait Req3Conflict { def three: String }

            val prereqs = Registry()
                .register[Req3]( () => Right( new Req3 {
                    val three = "3"
                } ) )
                .register[Req3Conflict]( () => Right( new Req3Conflict {
                    val three = "Troix"
                } ) )

            prereqs[Req3, Req3Conflict] ( (prereq) => Response() ) must throwA[
                Registry.ConflictingPrereqs
            ]
        }

        "ignore toString methods" in {
            trait Req4 { override def toString = "Req4" }
            trait Req5 { override def toString = "Req5" }

            val prereqs = Registry()
                .register[Req4]( () => Right( new Req4 {} ) )
                .register[Req5]( () => Right( new Req5 {} ) )

            prereqs[Req4, Req5] ( (prereq) => Response() )

            ok
        }
    }

    "A request with a succesfully processed prereq" should {
        "return a processed response" in {
            val prereqs = Registry()
                .register[Req1]( () => Right( new Req1{ val one = "1" } ) )
                .register[Req2]( () => Right( new Req2{ val two = "2" } ) )
                .register[Req3]( () => Right( new Req3{ val three = "3" } ) )

            val handler = prereqs[Req1, Req2, Req3]( (reqs) => {
                reqs.request must_== request
                reqs.one must_== "1"
                reqs.two must_== "2"
                reqs.three must_== "3"
                response
            } )

            handler.handle( request ) must_== response
        }
    }

    "A prereq with dependencies" should {
        "be able to access those dependencies" in {
            val prereqs = Registry()
                .register[Req1](
                    (bundle: Bundle) => {
                        bundle.get[Req2].two must_== "2"
                        bundle.get[Req3].three must_== "3"
                        Right( new Req1{ val one = "1" } )
                    },
                    classOf[Req2],
                    classOf[Req3]
                )
                .register[Req2]( () => Right( new Req2{ val two = "2" } ) )
                .register[Req3]( () => Right( new Req3{ val three = "3" } ) )

            val handler = prereqs[Req1]( (reqs) => response )

            handler.handle( request ) must_== response
        }
    }

    "A request with a failing prereq" should {

        val prereqs = Registry()
            .register[Req1]( () => Left( response ) )
            .register[Req2](
                (bundle: Bundle) => throw new Exception("Should not be called"),
                classOf[Req1]
            )

        "return the prereq response" in {
            val handler = prereqs[Req1]( (reqs) => {
                throw new Exception("Callback should not be invoked")
            } )

            handler.handle( request ) must_== response
        }

        "return the response from a failing dependency" in {
            val handler = prereqs[Req2]( (reqs) => {
                throw new Exception("Callback should not be invoked")
            } )

            handler.handle( request ) must_== response
        }

    }

}


