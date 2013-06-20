package test.scala.com.skene

import org.specs2.mutable._

import com.roundeights.skene._
import javax.servlet.http.{Cookie => JavaCookie}

class CookieTest extends Specification {

    "Converting a Cookie to a JavaCookie" should {

        "Enforce reasonable defaults" in {
            val cookie = Cookie("key", "value").toJavaCookie
            cookie.getName must_== "key"
            cookie.getValue must_== "value"
            cookie.getMaxAge must_== -1
            cookie.getDomain must_== null
            cookie.getPath must_== null
            cookie.getSecure must_== false
            cookie.isHttpOnly must_== true
        }

        "Fill values when defined" in {
            val cookie = Cookie(
                "key", "value", ttl = Some(500),
                domain = Some("example.com"), path = Some("/test/path"),
                secure = true, httpOnly = false
            ).toJavaCookie

            cookie.getName must_== "key"
            cookie.getValue must_== "value"
            cookie.getMaxAge must_== 500
            cookie.getDomain must_== "example.com"
            cookie.getPath must_== "/test/path"
            cookie.getSecure must_== true
            cookie.isHttpOnly must_== false
        }

    }

    "Converting a Java Cookie to a Cookie" should {

        "Convert empty values" in {
            Cookie( new JavaCookie("Name", "Value") ) must_==
                Cookie( "Name", "Value", httpOnly = false )
        }

        "Fill defined values" in {
            val cookie = new JavaCookie("Name", "Value")
            cookie.setMaxAge(500)
            cookie.setDomain("example.com")
            cookie.setPath("/path")
            cookie.setHttpOnly( true )
            cookie.setSecure( true )

            Cookie( cookie ) must_== Cookie(
                "Name", "Value", Some(500),
                Some("example.com"), Some("/path"),
                true, true
            )
        }
    }

}


