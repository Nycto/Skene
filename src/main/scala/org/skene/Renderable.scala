package org.skene

import java.io.Writer
import java.lang.StringBuilder
import java.lang.StringBuffer

import scala.xml.NodeSeq

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
 * Helper functions for creating renderable objects
 */
object Renderable {

    /**
     * A class for converting a callback to a renderable
     */
    class CallbackRenderer (
        private val callback: (Writer) => Unit
    ) extends Renderable {

        /**
         * Constructor...
         */
        def this ( thunk: () => String )
            = this( _.write( thunk() ) )

        /**
         * @see Renderable
         */
        override def render ( output: Writer ) = callback( output )
    }

    /**
     * A class to convert a string to a renderable
     */
    class StringRenderer ( private val content: String ) extends Renderable {
        /**
         * @see Renderable
         */
        override def render ( output: Writer) = output.write( content )
    }

    implicit def apply ( content: String ): Renderable
        = new StringRenderer( content )

    implicit def apply ( content: StringBuilder ): Renderable
        = new CallbackRenderer(() => content.toString )

    implicit def apply ( content: StringBuffer ): Renderable
        = new CallbackRenderer(() => content.toString )

    implicit def apply ( thunk: () => String ): Renderable
        = new CallbackRenderer( thunk )

    implicit def apply ( callback: (Writer) => Unit ): Renderable
        = new CallbackRenderer( callback )

    implicit def apply ( content: NodeSeq ): Renderable
        = new CallbackRenderer(() => content.toString )
}

