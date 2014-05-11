package test.scala.com.skene

import org.specs2.mutable._
import org.specs2.mock._

import java.io.{ByteArrayOutputStream, StringReader, ByteArrayInputStream}
import scala.io.Codec

import com.roundeights.skene._

class RenderableTest extends Specification with Mockito {

    private def assertRenders (
        renderable: Renderable, vs: String = "<p>Data</p>"
    ) = {
        val stream = new ByteArrayOutputStream
        renderable.render(stream, Codec.UTF8)
        val result: String = stream.toString("UTF8")
        result.getBytes("UTF8") must_== vs.getBytes("UTF8")
    }

    "Renderable objects" should {

        "render from a String" in {
            assertRenders( Renderable("<p>Data</p>") )

            assertRenders(
                Renderable("<p>Data that you might render</p>"),
                "<p>Data that you might render</p>"
            )
        }

        "render from a StringBuilder" in {
            assertRenders( Renderable( new StringBuilder("<p>Data</p>") ) )
        }

        "render from a StringBuffer" in {
            assertRenders( Renderable( new StringBuffer("<p>Data</p>") ) )
        }

        "render from a Callback" in {
            assertRenders( Renderable(() => "<p>Data</p>" ) )
        }

        "render from a PrintStream" in {
            assertRenders( Renderable((print) => print.print("<p>Data</p>")) )
        }

        "render from an XML block" in {
            assertRenders( Renderable(<p>Data</p>) )
        }

        "render from a Reader" in {
            assertRenders( Renderable(new StringReader("<p>Data</p>")) )
        }

        "render from an InputStream" in {
            assertRenders( Renderable(new ByteArrayInputStream(
                "<p>Data</p>".getBytes("UTF8")
            )) )
        }

    }

}

