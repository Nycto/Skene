package test.scala.com.skene

import org.specs2.mutable._

import com.roundeights.skene._

class HeadersTest extends Specification {

    val headers = Headers(
        "One" -> "First",
        "Two" -> "Second",
        "One" -> "Une"
    )

    "A Headers object" should {

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
}


