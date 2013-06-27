package test.scala.com.skene.static

import org.specs2.mutable._

import com.roundeights.skene.static._
import com.roundeights.skene.Response.ContentType
import java.io.File

class AssetTest extends Specification {

    "An Asset" should {

        "Return its extension" in {
            Asset("/", "path/test.java").ext must_== Some(".java")
            Asset("/", "path/test.").ext must_== Some(".")
            Asset("/", "path/.test").ext must_== None
            Asset("/", "path/test").ext must_== None
            Asset("/", "pa.th/test").ext must_== None
            Asset("/", "test.java").ext must_== Some(".java")
            Asset("/", "test.").ext must_== Some(".")
            Asset("/", ".test").ext must_== None
            Asset("/", "test").ext must_== None
        }

        "Strip its extension" in {
            Asset("/", "path/test.java").stripExt must_== "path/test"
            Asset("/", "path/test.").stripExt must_== "path/test"
            Asset("/", "path/.test").stripExt must_== "path/.test"
            Asset("/", "path/test").stripExt must_== "path/test"
            Asset("/", "test.java").stripExt must_== "test"
            Asset("/", "test.").stripExt must_== "test"
            Asset("/", ".test").stripExt must_== ".test"
            Asset("/", "test").stripExt must_== "test"
        }

        "Add a version" in {
            Asset("/", "test.js").versioned("abc") must_== "test.abc.js"
            Asset("/", "test").versioned("abc") must_== "test.abc"
        }

        "Return a mime type" in {
            Asset("/", "test.html").mimeType must_== Some(ContentType.Html())
            Asset("/", "test.css").mimeType must_== Some(ContentType.Css())
            Asset("/", "test.unrecognized").mimeType must_== None
            Asset("/", "test").mimeType must_== None
        }

        "Canonicalize relative paths" in {
            Asset("/", "/test.html").path must_== "test.html"
            Asset("/", "../test.html").path must_== "test.html"
            Asset("/", "path/../../test.html").path must_== "test.html"
            Asset("/", "/path///test.html").path must_== "path/test.html"
            Asset("/", "path/././test.html").path must_== "path/test.html"
            Asset("/", "path/././test.html").path must_== "path/test.html"
            Asset("/", "./..//../.").path must_== ""
        }
    }

}


