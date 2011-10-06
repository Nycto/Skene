package test.scala.com.skene.matcher

import org.specs2.mutable._
import org.specs2.mock.Mockito

import org.skene._

class PathTest extends Specification with Mockito {

    val context = mock[Context];
    context.url returns URL("http://example.com/path/to/resource")

    "A Path matcher without wildcards" should {

        "match a full path" in {
            Matcher.path("/path/to/resource")
                .matches(context) must_== Matcher.Result(true)
        }

        "not match a different path" in {
            Matcher.path("/some/other/path")
                .matches(context) must_== Matcher.Result(false)
        }

        "not care about a leading slash" in {
            Matcher.path("path/to/resource")
                .matches(context) must_== Matcher.Result(true)
        }
    }

    "A Path matcher with wildcards" should {

        "handle a wildcard at the end of a pattern" in {
            Matcher.path("/path/to/*")
                .matches(context) must_== Matcher.Result(
                    true, Map("0" -> "resource")
                )

            Matcher.path("/path/*/*")
                .matches(context) must_== Matcher.Result(
                    true, Map("0" -> "to", "1" -> "resource")
                )
        }

        "shouldnt consume a trailing slash with a trailing wildcard" in {
            val trailing = mock[Context];
            trailing.url returns URL("http://example.com/path/to/resource/")

            Matcher.path("/path/*/*")
                .matches(trailing) must_== Matcher.Result(false)
        }

        "match a path with various wildcard placements" in {
            Matcher.path("/*/to/resource")
                .matches(context) must_== Matcher.Result(
                    true, Map("0" -> "path")
                )

            Matcher.path("/path/*/resource")
                .matches(context) must_== Matcher.Result(
                    true, Map("0" -> "to")
                )

            Matcher.path("/*/*/resource")
                .matches(context) must_== Matcher.Result(
                    true, Map("0" -> "path", "1" -> "to")
                )

            Matcher.path("/*/*/*")
                .matches(context) must_== Matcher.Result(
                    true, Map("0" -> "path", "1" -> "to", "2" -> "resource")
                )
        }

        "not care about multiple wildcards in a row" in {
            Matcher.path("/**/to/resource")
                .matches(context) must_== Matcher.Result(
                    true, Map("0" -> "path")
                )

            Matcher.path("/**/****/resource")
                .matches(context) must_== Matcher.Result(
                    true, Map("0" -> "path", "1" -> "to")
                )

            Matcher.path("/path/**/**")
                .matches(context) must_== Matcher.Result(
                    true, Map("0" -> "to", "1" -> "resource")
                )
        }

        "not care about a leading slash" in {
            Matcher.path("path/to/*")
                .matches(context) must_== Matcher.Result(
                    true, Map("0" -> "resource")
                )
        }

        "not match incorrect patterns" in {
            Matcher.path("/*/to")
                .matches(context) must_== Matcher.Result(false)
            Matcher.path("/*/*")
                .matches(context) must_== Matcher.Result(false)
            Matcher.path("/*/*/*/*")
                .matches(context) must_== Matcher.Result(false)
            Matcher.path("/path/*/other")
                .matches(context) must_== Matcher.Result(false)
        }
    }

    "A named path matcher" should {

        "treat a named parameter like a glob" in {
            Matcher.path("/path/to/:name")
                .matches(context) must_== Matcher.Result(
                    true, Map("name" -> "resource")
                )

            Matcher.path("/path/:one/:two")
                .matches(context) must_== Matcher.Result(
                    true, Map("one" -> "to", "two" -> "resource")
                )

            Matcher.path("/:path/to/resource")
                .matches(context) must_== Matcher.Result(
                    true, Map("path" -> "path")
                )
        }

        "ignore extra colons" in {
            Matcher.path("/path/to/:::name")
                .matches(context) must_== Matcher.Result(
                    true, Map("name" -> "resource")
                )

            Matcher.path("/path/::one::/::::two:")
                .matches(context) must_== Matcher.Result(
                    true, Map("one" -> "to", "two" -> "resource")
                )
        }

        "not care about a leading slash" in {
            Matcher.path(":path/to/resource")
                .matches(context) must_== Matcher.Result(
                    true, Map("path" -> "path")
                )
        }

        "allow numbers, letters and underscores in names" in {
            Matcher.path(":path_123/to/resource")
                .matches(context) must_== Matcher.Result(
                    true, Map("path_123" -> "path")
                )
        }

        "not allow blank names" in {
            Matcher.path("path/:/:")
                .matches(context) must_== Matcher.Result(false)
        }

        "not match incorrect patterns" in {
            Matcher.path("/:one/to")
                .matches(context) must_== Matcher.Result(false)
            Matcher.path("/:one/:two")
                .matches(context) must_== Matcher.Result(false)
            Matcher.path("/:1/:2/:3/:4")
                .matches(context) must_== Matcher.Result(false)
            Matcher.path("/path/:one/other")
                .matches(context) must_== Matcher.Result(false)
        }
    }
}

