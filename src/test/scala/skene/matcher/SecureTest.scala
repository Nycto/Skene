package test.scala.com.skene.matcher

import org.specs2.mutable._

import com.roundeights.skene._

class SecureTest extends Specification {

    "A Secure Matcher" should {

        "pass when the isSecure flag of a request matches" in {
            Matcher.isSecure.matches( BareRequest(isSecure = true) ) must_==
                Matcher.Result(true)

            Matcher.notSecure.matches( BareRequest(isSecure = false) ) must_==
                Matcher.Result(true)
        }

        "fail when the isSecure flag of a request doesnt match" in {
            Matcher.isSecure.matches( BareRequest(isSecure = false) ) must_==
                Matcher.Result(false)

            Matcher.notSecure.matches( BareRequest(isSecure = true) ) must_==
                Matcher.Result(false)
        }

    }
}

