package org.skene

import java.io.Writer
import java.lang.StringBuilder
import java.lang.StringBuffer

import scala.xml.NodeSeq

/**
 * Helper functions for creating renderable objects
 */
object Renderable {

    implicit def apply ( content: String ): Renderable
        = new StringRenderer( content )

    implicit def apply ( content: StringBuilder ): Renderable
        = new CallbackRenderer({ content.toString })

    implicit def apply ( content: StringBuffer ): Renderable
        = new CallbackRenderer({ content.toString })

    implicit def apply ( thunk: () => String ): Renderable
        = new CallbackRenderer( thunk )

    implicit def apply ( content: NodeSeq ): Renderable
        = new CallbackRenderer({ content.toString })
}

/**
 * A piece of data that can be rendered down to a string
 */
trait Renderable {
    /**
     * Renders this object back to the given Writer
     */
    def render ( output: Writer ): Unit
}

/**
 * A class to convert a string to a renderable
 */
class StringRenderer ( private val content: String ) extends Renderable {
    override def render ( output: Writer) = output.write( content )
}

/**
 * A class for converting a callback to a renderable
 */
class CallbackRenderer ( private val callback: () => String )
    extends Renderable
{
    override def render ( output: Writer ) = output.write( callback() )
}

