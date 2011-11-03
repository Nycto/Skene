package test.scala.com.skene

import org.specs2.mutable._

import org.skene._

class RequestTest extends Specification {

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

    "The Request.Method enum" should {

        "be buildable from a string" in {
            Request.Method("GET") must_== Request.Method.GET()
            Request.Method("Post") must_== Request.Method.POST()
            Request.Method("Um") must_== Request.Method.UNKNOWN("Um")

            Request.Method("Post") must_!= Request.Method.GET()
        }

        "convert to strings" in {
            Request.Method.GET().toString must_== "GET"
            Request.Method.POST().toString must_== "POST"
            Request.Method.DELETE().toString must_== "DELETE"
            Request.Method.PUT().toString must_== "PUT"
        }

        "respond well to pattern matching" in {
            Request.Method("GET") match {
                case Request.Method.POST() => ko
                case Request.Method.UNKNOWN(_) => ko
                case Request.Method.GET() => ok
                case _ => ko
            }
        }

    }

}

