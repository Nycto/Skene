package test.scala.com.skene

import org.specs2.mutable._
import org.specs2.mock._

import org.skene._

class ParameterizedRequestTest extends Specification with Mockito {

    val request = BareRequest (
        params = Map[String, String]("1" -> "A", "2" -> "B")
    )

    "The Request.withParams method" should {

        "override the parameters in an existing request" in {
            request.withParams( Map("2" -> "!", "3" -> "C" ) )
                .params must_== Map( "1" -> "A", "2" -> "!", "3" -> "C" )

            request.withParams( "2" -> "!", "3" -> "C" )
                .params must_== Map( "1" -> "A", "2" -> "!", "3" -> "C" )
        }
    }
}
