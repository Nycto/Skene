package test.scala.com.skene.matcher

import org.specs2.mutable._

import com.roundeights.skene._

class MethodTest extends Specification {

    val request = BareRequest()

    "A Method Matcher" should {

        "pass when the method of a request matches" in {
            val matcher = Matcher.method( Request.Method.GET )
            matcher.matches( request ) must_== Matcher.Result(true)
        }

        "fail when the method of a request doesnt match" in {
            val matcher = Matcher.method( Request.Method.POST )
            matcher.matches( request ) must_== Matcher.Result(false)
        }

    }

}

