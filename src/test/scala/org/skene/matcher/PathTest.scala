package test.scala.com.skene.matcher

import org.specs2.mutable._
import org.specs2.mock.Mockito

import org.skene._

class PathTest extends Specification with Mockito {

    val context = mock[Context];
    context.url returns URL("http://example.com/path/to/resource")

    "A Path matcher without wildcards" should {

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

    "A Path matcher with wildcards" should {

        "handle a wildcard at the end of a pattern" in {
            Matcher.path("/path/to/*").matches(context) must_== true
            Matcher.path("/path/*/*").matches(context) must_== true
        }

        "shouldnt consume a trailing slash with a trailing wildcard" in {
            val trailing = mock[Context];
            trailing.url returns URL("http://example.com/path/to/resource/")

            Matcher.path("/path/*/*").matches(trailing) must_== false
        }

        "match a path with various wildcard placements" in {
            Matcher.path("/*/to/resource").matches(context) must_== true
            Matcher.path("/path/*/resource").matches(context) must_== true
            Matcher.path("/*/*/resource").matches(context) must_== true
            Matcher.path("/*/*/*").matches(context) must_== true
        }

        "not care about multiple wildcards in a row" in {
            Matcher.path("/**/to/resource").matches(context) must_== true
            Matcher.path("/**/****/resource").matches(context) must_== true
            Matcher.path("/path/**/**").matches(context) must_== true
        }

        "not care about a leading slash" in {
            Matcher.path("path/to/*").matches(context) must_== true
        }

        "not match incorrect patterns" in {
            Matcher.path("/*/to").matches(context) must_== false
            Matcher.path("/*/*").matches(context) must_== false
            Matcher.path("/*/*/*/*").matches(context) must_== false
            Matcher.path("/path/*/other").matches(context) must_== false
        }
    }
}

