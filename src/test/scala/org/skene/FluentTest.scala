package test.scala.com.skene

import org.specs2.mutable._
import org.specs2.mock._

import org.skene._

class SkeneTest extends Specification with Mockito {

    val indexPage = Renderable("index")
    val specificPage = Renderable("specificPage")
    val defaultPage = Renderable("default")

    val interface = new Skene {
        index { indexPage }
        request ("/specific") { specificPage }
        default { defaultPage }
    }

    "A fluent interface" should {

        "pick up the index request" in {
            val context = mock[Context];
            context.url returns URL("http://example.com")

            interface.handle(context) must_== indexPage
        }

        "pick up specific pages" in {
            val context = mock[Context];
            context.url returns URL("http://example.com/specific")

            interface.handle(context) must_== specificPage
        }

        "fall back on the default handler when no page matches" in {
            val context = mock[Context];
            context.url returns URL("http://example.com/undefined")

            interface.handle(context) must_== defaultPage
        }
    }

}

