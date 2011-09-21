package test.scala.com.skene.matcher

import org.specs2.mutable._
import org.specs2.mock.Mockito

import org.skene._

class MatcherTest extends Specification with Mockito {

    "A Path matcher" should {

        val context = mock[Context];
        context.url returns URL("http://example.com/path/to/resource")

        "match a full path" in {
            Matcher.path("/path/to/resource").matches(context) must_== true
        }

        "not match a different path" in {
            Matcher.path("/some/other/path").matches(context) must_== false
        }

        "not care about a leading slash" in {
            Matcher.path("path/to/resource").matches(context) must_== true
        }
    }

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

