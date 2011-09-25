package test.scala.com.skene.matcher

import org.specs2.mutable._
import org.specs2.mock.Mockito

import org.skene._

class MatcherTest extends Specification with Mockito {

    "An Always matcher" should {
        "always return true" in {
            Matcher.always.matches( mock[Context] ) must_== true
        }
    }

    "A Never matcher" should {
        "always return false" in {
            Matcher.never.matches( mock[Context] ) must_== false
        }
    }
}

