package test.scala.com.skene.matcher

import org.specs2.mutable._
import org.specs2.mock.Mockito

import org.skene._

class PathTest extends Specification with Mockito {

    val request = mock[Request];
    request.url returns URL("http://example.com/path/to/resource")

    "A Path matcher without wildcards" should {

        "match a full path" in {
            Matcher.path("/path/to/resource")
                .matches(request) must_== Matcher.Result(true)
        }

        "not match a different path" in {
            Matcher.path("/some/other/path")
                .matches(request) must_== Matcher.Result(false)
        }

        "not care about a leading slash" in {
            Matcher.path("path/to/resource")
                .matches(request) must_== Matcher.Result(true)
        }
    }

    "A Path matcher with wildcards" should {

        "handle a wildcard at the end of a pattern" in {
            Matcher.path("/path/to/*")
                .matches(request) must_== Matcher.Result(
                    true, Map("0" -> "resource")
                )

            Matcher.path("/path/*/*")
                .matches(request) must_== Matcher.Result(
                    true, Map("0" -> "to", "1" -> "resource")
                )
        }

        "shouldnt consume a trailing slash with a trailing wildcard" in {
            val trailing = mock[Request];
            trailing.url returns URL("http://example.com/path/to/resource/")

            Matcher.path("/path/*/*")
                .matches(trailing) must_== Matcher.Result(false)
        }

        "match a path with various wildcard placements" in {
            Matcher.path("/*/to/resource")
                .matches(request) must_== Matcher.Result(
                    true, Map("0" -> "path")
                )

            Matcher.path("/path/*/resource")
                .matches(request) must_== Matcher.Result(
                    true, Map("0" -> "to")
                )

            Matcher.path("/*/*/resource")
                .matches(request) must_== Matcher.Result(
                    true, Map("0" -> "path", "1" -> "to")
                )

            Matcher.path("/*/*/*")
                .matches(request) must_== Matcher.Result(
                    true, Map("0" -> "path", "1" -> "to", "2" -> "resource")
                )
        }

        "not care about multiple wildcards in a row" in {
            Matcher.path("/**/to/resource")
                .matches(request) must_== Matcher.Result(
                    true, Map("0" -> "path")
                )

            Matcher.path("/**/****/resource")
                .matches(request) must_== Matcher.Result(
                    true, Map("0" -> "path", "1" -> "to")
                )

            Matcher.path("/path/**/**")
                .matches(request) must_== Matcher.Result(
                    true, Map("0" -> "to", "1" -> "resource")
                )
        }

        "not care about a leading slash" in {
            Matcher.path("path/to/*")
                .matches(request) must_== Matcher.Result(
                    true, Map("0" -> "resource")
                )
        }

        "not match incorrect patterns" in {
            Matcher.path("/*/to")
                .matches(request) must_== Matcher.Result(false)
            Matcher.path("/*/*")
                .matches(request) must_== Matcher.Result(false)
            Matcher.path("/*/*/*/*")
                .matches(request) must_== Matcher.Result(false)
            Matcher.path("/path/*/other")
                .matches(request) must_== Matcher.Result(false)
        }
    }

    "A named path matcher" should {

        "treat a named parameter like a glob" in {
            Matcher.path("/path/to/:name")
                .matches(request) must_== Matcher.Result(
                    true, Map("name" -> "resource")
                )

            Matcher.path("/path/:one/:two")
                .matches(request) must_== Matcher.Result(
                    true, Map("one" -> "to", "two" -> "resource")
                )

            Matcher.path("/:path/to/resource")
                .matches(request) must_== Matcher.Result(
                    true, Map("path" -> "path")
                )
        }

        "ignore extra colons" in {
            Matcher.path("/path/to/:::name")
                .matches(request) must_== Matcher.Result(
                    true, Map("name" -> "resource")
                )

            Matcher.path("/path/::one::/::::two:")
                .matches(request) must_== Matcher.Result(
                    true, Map("one" -> "to", "two" -> "resource")
                )
        }

        "not care about a leading slash" in {
            Matcher.path(":path/to/resource")
                .matches(request) must_== Matcher.Result(
                    true, Map("path" -> "path")
                )
        }

        "allow numbers, letters and underscores in names" in {
            Matcher.path(":path_123/to/resource")
                .matches(request) must_== Matcher.Result(
                    true, Map("path_123" -> "path")
                )
        }

        "not allow blank names" in {
            Matcher.path("path/:/:")
                .matches(request) must_== Matcher.Result(false)
        }

        "not match incorrect patterns" in {
            Matcher.path("/:one/to")
                .matches(request) must_== Matcher.Result(false)
            Matcher.path("/:one/:two")
                .matches(request) must_== Matcher.Result(false)
            Matcher.path("/:1/:2/:3/:4")
                .matches(request) must_== Matcher.Result(false)
            Matcher.path("/path/:one/other")
                .matches(request) must_== Matcher.Result(false)
        }
    }
}

