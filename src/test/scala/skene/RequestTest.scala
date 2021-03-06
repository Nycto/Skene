package test.scala.com.skene

import org.specs2.mutable._

import com.roundeights.skene._
import java.util.Date

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

    "The Request Method Accessor methods" should {

        "Respond to the GET method" in {
            val request = BareRequest( method = Request.Method.GET )
            request.isGet must_== true
            request.isPost must_== false
            request.isDelete must_== false
            request.isPut must_== false
        }

        "Respond to the POST method" in {
            val request = BareRequest( method = Request.Method.POST )
            request.isGet must_== false
            request.isPost must_== true
            request.isDelete must_== false
            request.isPut must_== false
        }

        "Respond to the DELETE method" in {
            val request = BareRequest( method = Request.Method.DELETE )
            request.isGet must_== false
            request.isPost must_== false
            request.isDelete must_== true
            request.isPut must_== false
        }

        "Respond to the PUT method" in {
            val request = BareRequest( method = Request.Method.PUT )
            request.isGet must_== false
            request.isPost must_== false
            request.isDelete must_== false
            request.isPut must_== true
        }

    }

    "The Request QueryString methods" should {

        "Generate parameters from the query string" in {
            BareRequest().queryParams must_== QueryString()

            BareRequest( queryString = Some("") ).queryParams must_==
                QueryString()

            BareRequest( queryString = Some("a=b&c=d") ).queryParams must_==
                QueryString("a" -> "b", "c" -> "d")
        }

        "Generate parameters from body" in {
            BareRequest().bodyParams must_== QueryString()

            BareRequest( body = "" ).bodyParams must_== QueryString()

            BareRequest( body = "a=b&c=d" ).bodyParams must_==
                QueryString("a" -> "b", "c" -> "d")
        }
    }

    "The Request.Method enum" should {

        "be buildable from a string" in {
            Request.Method("GET") must_== Request.Method.GET
            Request.Method("Post") must_== Request.Method.POST
            Request.Method("Um") must_== Request.Method.UNKNOWN

            Request.Method("Post") must_!= Request.Method.GET
        }

        "convert to strings" in {
            Request.Method.GET.toString must_== "GET"
            Request.Method.POST.toString must_== "POST"
            Request.Method.DELETE.toString must_== "DELETE"
            Request.Method.PUT.toString must_== "PUT"
        }

        "respond well to pattern matching" in {
            Request.Method("GET") match {
                case Request.Method.POST => ko
                case Request.Method.UNKNOWN => ko
                case Request.Method.GET => ok
                case _ => ko
            }
            ok
        }

    }
}

