package test.scala.com.skene

import org.specs2.mutable._

import com.roundeights.skene.static._
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
    }

}


