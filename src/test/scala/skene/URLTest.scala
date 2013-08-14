package test.scala.com.skene

import org.specs2.mutable._

import com.roundeights.skene.URL

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

        "return the subdomain of a host" in {
            URL("https://example.com").subdomain must_== None
            URL("https://www.example.com").subdomain must_== None
            URL("https://one.example.com").subdomain must_== Some("one")
            URL("https://one.two.example.com").subdomain must_==
                Some("one.two")
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

    "Replacing the path of a URL" should {

        "Handle simple URLs" in {
            URL("https://www.example.com/path/to/file.ext")
                .withPath("/new_path") must_==
                    URL("https://www.example.com/new_path")

            URL("https://www.example.com/path/to/file.ext")
                .withPath("new_path") must_==
                    URL("https://www.example.com/new_path")

            URL("https://www.example.com")
                .withPath("new_path") must_==
                    URL("https://www.example.com/new_path")
        }

        "Leave protocols, hosts, ports, and query strings in place" in {
            URL("ftp://example.com:90/path?query")
                .withPath("/new_path") must_==
                    URL("ftp://example.com:90/new_path?query")
        }
    }

    "Prefixing the path of a URL" should {

        "Add to the existing path" in {
            URL("https://www.example.com/path/to/file.ext")
                .prefixPath("/new_path") must_==
                    URL("https://www.example.com/new_path/path/to/file.ext")
        }

        "Set the path if one doesn't exist" in {
            URL("https://www.example.com/")
                .prefixPath("/new_path") must_==
                    URL("https://www.example.com/new_path")

            URL("https://www.example.com")
                .prefixPath("/new_path") must_==
                    URL("https://www.example.com/new_path")
        }
    }
}

