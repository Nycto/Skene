package test.scala.com.skene

import org.specs2.mutable._
import org.specs2.mock._

import org.skene._

class SkeneTest extends Specification with Mockito {

    val indexPage = Response("index")
    val specificPage = Response("specificPage")
    val defaultPage = Response("default")

    val interface = new Skene {
        index { indexPage }
        request ("/specific") { specificPage }
        default { defaultPage }
    }

    "A fluent interface" should {

        "pick up the index request" in {
            val request = mock[Request];
            request.url returns URL("http://example.com")

            interface.handle(request) must_== indexPage
        }

        "pick up specific pages" in {
            val request = mock[Request];
            request.url returns URL("http://example.com/specific")

            interface.handle(request) must_== specificPage
        }

        "fall back on the default handler when no page matches" in {
            val request = mock[Request];
            request.url returns URL("http://example.com/undefined")

            interface.handle(request) must_== defaultPage
        }
    }

}

