package test.scala.com.skene

import org.specs2.mutable._

import com.roundeights.skene._
import java.util.Date

class HeadersTest extends Specification {

    "A Headers object" should {

        val headers = Headers(
            "One" -> "First",
            "Two" -> "Second",
            "One" -> "Une"
        )

        "Provide access to individual headers" in {
            headers("One") must_== Some("First")
            headers("Two") must_== Some("Second")
            headers("Third") must_== None
        }

        "Ignore case sensitivity" in {
            headers("one") must_== Some("First")
            headers("two") must_== Some("Second")
        }

        "Provide access to a list of header values" in {
            headers.get("One") must_== Seq("First", "Une")
            headers.get("Two") must_== Seq("Second")
            headers.get("Third") must_== Seq()
        }

        "Return whether a header is set" in {
            headers.contains("One") must_== true
            headers.contains("Two") must_== true
            headers.contains("Third") must_== false
        }
    }

    "The Headers.getDate method" should {
        val headers = Headers(
            "valid" -> "Sun, 23 Jun 2013 15:24:35 PDT",
            "invalid" -> "Some other string"
        )

        "Return None with when the header isn't set" in {
            headers.getDate("not set") must_== None
        }

        "Return None with when the header doesn't parse" in {
            headers.getDate("invalid") must_== None
        }

        "Return a Date when the header is a date" in {
            headers.getDate("valid") must_==
                Some(new Date(1372026275000L))
        }
    }

    "The Headers.contentType method" should {

        "Return None with when the header isn't set" in {
            val headers = Headers()
            headers.contentType must_== None
        }

        "Return the header when it is set" in {
            val headers = Headers("Content-Type" -> "text/plain")
            headers.contentType must_== Some("text/plain")
        }

        "Return the header when it is set, excluding the encoding" in {
            val headers = Headers("Content-Type" -> "text/plain; charset=UTF-8")
            headers.contentType must_== Some("text/plain")
        }
    }

    "The Headers.basicAuth method" should {

        "Return None with when the Auth header isn't set" in {
            Headers().basicAuth must_== None
        }

        "Return the credentials when set" in {
            Headers("Authorization" -> "Basic dXNlcjpwYXNz")
                .basicAuth must_== Some( "user" -> "pass" )

            Headers("Authorization" -> "  BASIC   dXNlcjpwYXNz  ")
                .basicAuth must_== Some( "user" -> "pass" )
        }

        "Return None when the method isn't basic" in {
            Headers("Authorization" -> "complex dXNlcjpwYXNz")
                .basicAuth must_== None
        }

        "Throw when the encoded value dosnt have a colon" in {
            Headers("Authorization" -> "Basic dXNlcnBhc3M=")
                .basicAuth must throwA[Headers.InvalidHeader]
        }

        "Throw when the value isnt base64 encoded" in {
            Headers("Authorization" -> "Basic !@#$%")
                .basicAuth must throwA[Headers.InvalidHeader]
        }
    }

    "The Headers.Accept object" in {

        "Parse a simple accepts header" in {
            new Headers.Accept("a/b, c/d").contentTypes must_==
                Set("a/b", "c/d")
        }

        "Parse an empty accepts header" in {
            new Headers.Accept("").contentTypes must_== Set()
            new Headers.Accept("   ").contentTypes must_== Set()
        }

        "Parse content type parameters" in {
            new Headers.Accept("a;1=2;3=4,b;5=6,c").params must_== Map(
                "a" -> List("1" -> "2", "3" -> "4"),
                "b" -> List("5" -> "6"),
                "c" -> List()
            )
        }

        "Ignore empty mime types with params" in {
            new Headers.Accept(";1=2,  ;3=4").contentTypes must_== Set()
        }

        "Ignore params with empty keys" in {
            new Headers.Accept("a;1=2;=3;4=5").params("a") must_==
                List( "1" -> "2", "4" -> "5" )
        }

        "Set the value to blank when a param is missing an equals" in {
            new Headers.Accept("a;1").params("a") must_== List( "1" -> "" )
        }

        "Trim down parameter keys" in {
            new Headers.Accept("a;  1  =  2  ").params("a") must_==
                List( "1" -> "  2  " )
        }

        "Returns whether a content type is accepted" in {
            val accept = new Headers.Accept("text/plain, application/json")
            accept("application/json") must_== true
            accept("TEXT/PLAIN") must_== true
            accept("text/html") must_== false
        }

        "Convert back to a string" in {
            new Headers.Accept("").toString must_== ""
            new Headers.Accept(
                "application/json,text/javascript,*/*;q=0.01").toString must_==
                "*/*; q=0.01, application/json, text/javascript"
        }
    }
}


