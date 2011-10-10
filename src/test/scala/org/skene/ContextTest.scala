package test.scala.com.skene

import org.specs2.mutable._
import org.specs2.mock._

import org.skene._

class ParameterizedContextTest extends Specification with Mockito {

    val context = BareContext (
        params = Map[String, String]("1" -> "A", "2" -> "B")
    )

    "The Context.withParams method" should {

        "override the parameters in an existing context" in {
            context.withParams( Map("2" -> "!", "3" -> "C" ) )
                .params must_== Map( "1" -> "A", "2" -> "!", "3" -> "C" )

            context.withParams( "2" -> "!", "3" -> "C" )
                .params must_== Map( "1" -> "A", "2" -> "!", "3" -> "C" )
        }
    }
}
