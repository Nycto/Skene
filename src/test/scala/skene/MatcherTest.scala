package test.scala.com.skene.matcher

import org.specs2.mutable._

import com.roundeights.skene._

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
            ( Matcher.Result(true, "1" -> "one", "2" -> "two")
                + Matcher.Result(true, "2" -> "deux", "3" -> "trois")
            ).params must_== Map("1" -> "one", "2" -> "deux", "3" -> "trois")
        }

    }

}

