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

}


