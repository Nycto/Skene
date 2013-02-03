package com.roundeights.skene

import org.specs2.mutable._
import org.specs2.mock._

class BundleTest extends Specification with Mockito {

    trait Req1 { def one: String }
    trait Req2 { def two: String }
    trait Req3 { def three: String }

    val bundle = new Bundle()
        .add[Req1]( new Req1 { override def one = "1" } )
        .add[Req2]( new Req2 { override def two = "2" } )

    "A bundle" should {

        "convert to an instance of what it contains" in {
            val proxy: Req1 with Req2 = bundle.asProxyOf[Req1 with Req2](
                List(classOf[Req1], classOf[Req2])
            )

            proxy.one must_== "1"
            proxy.two must_== "2"
        }

        "throw when trying to add a mismatched class/value" in {
            val req2 = new Req2 { override def two = "2" };
            bundle.add( classOf[Req1], req2 ) must throwA[Exception]
        }

        "throw when casting to a class it doesnt contain" in {
            bundle.asProxyOf[Req3](List(classOf[Req3])) must throwA[Exception]
        }

    }

}


