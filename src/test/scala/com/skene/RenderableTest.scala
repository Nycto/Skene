package test.scala.com.skene

import org.specs2.mutable._
import org.specs2.mock._

import java.io.Writer
import java.lang.StringBuilder
import java.lang.StringBuffer

import main.scala.com.skene._

class RenderableTest extends Specification with Mockito {

    private def assertRenders ( renderable: Renderable ) = {
        val writer = mock[Writer]
        renderable.render(writer)
        there was one(writer).write("<p>Data</p>")
    }

    "Renderable objects" should {
        "render from a String" in {
            assertRenders( Renderable("<p>Data</p>") )
        }
        "render from a StringBuilder" in {
            assertRenders( Renderable( new StringBuilder("<p>Data</p>") ) )
        }
        "render from a StringBuffer" in {
            assertRenders( Renderable( new StringBuffer("<p>Data</p>") ) )
        }
        "render from a Thunk" in {
            assertRenders( Renderable(() => { "<p>Data</p>" }) )
        }
        "render from an XML block" in {
            assertRenders( Renderable(<p>Data</p>) )
        }
    }
}

