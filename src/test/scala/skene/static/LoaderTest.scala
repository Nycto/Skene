package test.scala.com.skene.static

import org.specs2.mutable._
import org.specs2.mock._

import scala.concurrent.ExecutionContext.Implicits.global
import com.roundeights.skene.static._
import java.io.File

class AssetLoaderTest extends Specification with Mockito {

    val assetFile = mock[File]

    val asset = mock[Asset]
    asset.file returns assetFile
    asset.stripExt returns "stripped/file"
    asset.ext returns Some(".ext")

    "An AssetLoader" should {

        "Generate a URL with a hash" in {
            val hasher = mock[HashCache]
            hasher.hash( any[File] ) returns Some("ABCDEFGHIJKLMNOP")

            val loader = new AssetLoader( "asset", hasher, (path:String) => {
                path must_== "path/file.js"
                Some(asset)
            })

            loader.url("/path/file.js") must_==
                Some("/asset/stripped/file.ABCDEFGH.ext")
        }

        "Normalize the prefix" in {
            val hasher = mock[HashCache]
            hasher.hash( any[File] ) returns Some("ABCDEFGHIJKLMNOP")

            new AssetLoader( "/asset/./path/../", hasher, _ => Some(asset) )
                .url("/path/file.js") must_==
                Some("/asset/stripped/file.ABCDEFGH.ext")
        }

        "Return None if the file can't be found" in {
            new AssetLoader("asset", mock[HashCache], _ => None)
                .url("/path/file.js") must_== None
        }

        "Return None if the hash can't be generated" in {
            val hasher = mock[HashCache]
            hasher.hash( any[File] ) returns None

            new AssetLoader( "asset", hasher, _ => Some(asset) )
                .url("/path/file.js") must_== None
        }
    }
}

