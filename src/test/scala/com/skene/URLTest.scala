package test.scala.com.skene

import org.specs2.mutable._

import main.scala.com.skene.URL

class URLTest extends Specification {

    "A URL" should {

        val url = URL("http://example.com")

        "equal itself" in {
            url must_== url
        }

        "equal an equivalent URL" in {
            url must_== URL("http://example.com")
        }

        "not equal a different URL" in {
            url must_!= URL("http://subdomain.example.com")
        }

        "be convertable to a string" in {
            url.toString must_== "http://example.com"
        }

        "be usable in a pattern matcher" in {
            val matches = url match {
                case URL("http://example.com") => true
                case _ => false
            }

            matches must_== true
        }

    }

    "The host accessor of a URL" should {
        "return the host of a URL" in {
            URL("https://www.example.com:8080")
                .host must_== "www.example.com"
        }
    }

    "The path accessor of a URL" should {

        "return the path when one exists" in {
            URL("https://www.example.com/path/to/file.ext")
                .path must_== Some("/path/to/file.ext")
        }

        "return None when the path doesn't exist" in {
            URL("https://www.example.com").path must_== None
        }
    }
}

