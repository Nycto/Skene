package test.scala.com.skene.matcher

import org.specs2.mutable._

import com.roundeights.skene._

class AbsoluteTest extends Specification {

    val request = BareRequest()

    "An Always matcher" should {
        "always return true" in {
            Matcher.always.matches( request ) must_== Matcher.Result(true)
        }

        "include any parameters it was given" in {
            Matcher.always("1" -> "one")
                .matches(request) must_== Matcher.Result(true, "1" -> "one")
        }
    }

    "A Never matcher" should {
        "always return false" in {
            Matcher.never.matches( request ) must_== Matcher.Result(false)
        }
    }

}

