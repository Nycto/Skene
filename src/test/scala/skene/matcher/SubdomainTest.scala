package test.scala.com.skene.matcher

import org.specs2.mutable._

import com.roundeights.skene._

class SubdomainTest extends Specification {

    "A Subdomain Matcher" should {

        "match when the subdomain matches" in {
            Matcher.subdomain("sub").matches(
                BareRequest( url = URL("http://sub.example.com") )
            ) must_== Matcher.Result(true)

            Matcher.subdomain("SUB").matches(
                BareRequest( url = URL("http://sub.example.com") )
            ) must_== Matcher.Result(true)

            Matcher.noSubdomain.matches(
                BareRequest( url = URL("http://example.com") )
            ) must_== Matcher.Result(true)
        }

        "Fail when the subdomain doesn't match" in {
            Matcher.subdomain("different").matches(
                BareRequest( url = URL("http://sub.example.com") )
            ) must_== Matcher.Result(false)

            Matcher.subdomain("sub").matches(
                BareRequest( url = URL("http://example.com") )
            ) must_== Matcher.Result(false)

            Matcher.noSubdomain.matches(
                BareRequest( url = URL("http://sub.example.com") )
            ) must_== Matcher.Result(false)
        }

        "Treat www like an absent subdomain" in {
            Matcher.subdomain("www").matches(
                BareRequest( url = URL("http://www.example.com") )
            ) must_== Matcher.Result(true)

            Matcher.subdomain("www").matches(
                BareRequest( url = URL("http://example.com") )
            ) must_== Matcher.Result(true)

            Matcher.noSubdomain.matches(
                BareRequest( url = URL("http://www.example.com") )
            ) must_== Matcher.Result(true)
        }

        "Treat a blank string like an absent subdomain" in {
            Matcher.subdomain("").matches(
                BareRequest( url = URL("http://example.com") )
            ) must_== Matcher.Result(true)

            Matcher.subdomain("").matches(
                BareRequest( url = URL("http://www.example.com") )
            ) must_== Matcher.Result(true)
        }

    }
}

