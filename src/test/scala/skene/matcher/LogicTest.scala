package test.scala.com.skene.matcher

import org.specs2.mutable._
import com.roundeights.skene._
import java.lang.RuntimeException

class LogicTest extends Specification {

    val request = BareRequest()

    "An And Matcher" should {

        "pass when empty" in {
            val matcher = Matcher.and()
            matcher.matches( request ) must_== Matcher.Result(true)
        }

        "pass when all the contained matchers pass" in {
            val matcher = Matcher.and(
                Matcher.always, Matcher.always, Matcher.always
            )

            matcher.matches( request ) must_== Matcher.Result(true)
        }

        "fail when any of the contained matchers fail" in {
            val matcher = Matcher.and(
                Matcher.always,
                Matcher.always,
                Matcher.never,
                Matcher.call( (req) => {
                    throw new RuntimeException("Did not Short Circuit")
                })
            )

            matcher.matches( request ) must_== Matcher.Result(false)
        }

        "Collect parameters when all the matchers pass" in {
            val matcher = Matcher.and(
                Matcher.always( "1" -> "one", "2" -> "two" ),
                Matcher.always( "2" -> "deux", "3" -> "trois" )
            )

            matcher.matches( request ) must_== Matcher.Result(
                true, "1" -> "one", "2" -> "deux", "3" -> "trois"
            )
        }

        "Quash parameters when one of the matchers fails" in {
            val matcher = Matcher.and(
                Matcher.always( "1" -> "one", "2" -> "two" ),
                Matcher.never
            )

            matcher.matches( request ) must_== Matcher.Result( false )
        }

    }

    "An Or matcher" should {

        "fail when empty" in {
            val matcher = Matcher.or()
            matcher.matches( request ) must_== Matcher.Result(false)
        }

        "Pass when any of the contained matchers pass" in {
            val matcher = Matcher.or(
                Matcher.never,
                Matcher.never,
                Matcher.always( "1" -> "2" ),
                Matcher.call( (req) => {
                    throw new RuntimeException("Did not Short Circuit")
                })
            )

            matcher.matches(request) must_== Matcher.Result(true, "1" -> "2")
        }

        "Fail when all of the contained matchers fail" in {
            val matcher = Matcher.or( Matcher.never, Matcher.never )
            matcher.matches(request) must_== Matcher.Result(false)
        }

    }

}

