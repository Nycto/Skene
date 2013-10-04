package test.scala.com.skene.matcher

import org.specs2.mutable._

import com.roundeights.skene._

class HostTest extends Specification {

    "A Host Matcher" should {

        "match when the full host matches" in {
            Matcher.host("example.com").matches(
                BareRequest( url = URL("http://example.com") )
            ) must_== Matcher.Result(true)

            Matcher.host("example.com").matches(
                BareRequest( url = URL("http://EXAMPLE.com") )
            ) must_== Matcher.Result(true)

            Matcher.host("EXAMPLE.com").matches(
                BareRequest( url = URL("http://example.com") )
            ) must_== Matcher.Result(true)
        }

        "Fail when the host doesn't match" in {
            Matcher.host("different").matches(
                BareRequest( url = URL("http://example.com") )
            ) must_== Matcher.Result(false)

            Matcher.host("example.com").matches(
                BareRequest( url = URL("http://sub.example.com") )
            ) must_== Matcher.Result(false)

            Matcher.host("sub.example.com").matches(
                BareRequest( url = URL("http://example.com") )
            ) must_== Matcher.Result(false)
        }

        "Strip of www before comparing" in {
            Matcher.host("www.example.com").matches(
                BareRequest( url = URL("http://example.com") )
            ) must_== Matcher.Result(true)

            Matcher.host("example.com").matches(
                BareRequest( url = URL("http://www.example.com") )
            ) must_== Matcher.Result(true)
        }
    }
}


