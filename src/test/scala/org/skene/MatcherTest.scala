package test.scala.com.skene.matcher

import org.specs2.mutable._

import org.skene._

class MatcherTest extends Specification {

    val request = BareRequest()

    "Adding together two Matcher.Result objects" should {

        "Do a boolean And on the 'passed' parameter" in {
            (Matcher.Result(true) + Matcher.Result(true)).passed must_== true
            (Matcher.Result(false) + Matcher.Result(true)).passed must_== false
            (Matcher.Result(false) + Matcher.Result(false)).passed must_== false
            (Matcher.Result(true) + Matcher.Result(false)).passed must_== false
        }

        "Combine the parameters" in {
            ( Matcher.Result(true, Map("1" -> "one", "2" -> "two"))
                + Matcher.Result(true, Map("2" -> "deux", "3" -> "trois"))
            ).params must_== Map("1" -> "one", "2" -> "deux", "3" -> "trois")
        }

    }

    "An Always matcher" should {
        "always return true" in {
            Matcher.always.matches( request ) must_== Matcher.Result(true)
        }

        "include any parameters it was given" in {
            Matcher.always("1" -> "one")
                .matches(request) must_== Matcher.Result(true, Map("1" -> "one"))
        }
    }

    "A Never matcher" should {
        "always return false" in {
            Matcher.never.matches( request ) must_== Matcher.Result(false)
        }
    }

}

