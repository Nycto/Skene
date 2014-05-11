package com.roundeights.skene

import java.io.{InputStream, OutputStream, Reader, Writer, OutputStreamWriter}
import java.io.{File, FileInputStream, PrintStream}
import scala.xml.NodeSeq

import scala.io.Codec
import java.nio.CharBuffer

import scala.language.implicitConversions

/**
 * A piece of data that can be rendered down to an output stream
 */
trait Renderable {
    /** Renders this object back to the given Writer */
    def render ( output: OutputStream, codec: Codec ): Unit
}

/**
 * Helper functions for creating renderable objects
 */
object Renderable {

    /** Encodes a string to a byte array using the given encoding */
    def write ( content: String, to: OutputStream, using: Codec ): Unit = {
        val encoded = using.encoder.encode( CharBuffer.wrap(content) )
        to.write( encoded.array, encoded.arrayOffset, content.length )
    }

    /**
     * Uses a String as a renderable
     */
    class StringRenderer ( private val content: String ) extends Renderable {
        /** {@inheritDoc} */
        override def render ( output: OutputStream, codec: Codec )
            = write( content, output, codec )
    }

    /**
     * Uses a callback as a Renderable
     */
    class CallbackRenderer (
        private val callback: (OutputStream, Codec) => Unit
    ) extends Renderable {

        /** Constructor...  */
        def this ( callback: () => String )
            = this( (stream, codec) => write( callback(), stream, codec ) )

        /** Constructor...  */
        def this ( callback: (PrintStream) => Unit ) = this(
            (stream, codec) => callback(
                new PrintStream(stream, true, codec.name)
            )
        )

        /** {@inheritDoc} */
        override def render ( output: OutputStream, codec: Codec )
            = callback( output, codec )
    }

    /**
     * Converts any object with a toString method to a Renderable
     */
    class AnyRenderer ( private val obj: Any ) extends Renderable {
        /** {@inheritDoc} */
        override def render ( output: OutputStream, codec: Codec ): Unit
            = write( obj.toString, output, codec )
    }

    /**
     * Uses a Reader as an renderable
     */
    class ReaderRenderer ( private val content: Reader ) extends Renderable {

        /** {@inheritDoc} */
        override def render ( output: OutputStream, codec: Codec ) = {
            val buffer = new Array[Char](1024 * 4)
            val writer = new OutputStreamWriter(output, codec.charSet)

            def copy: Unit = {
                val read = content.read( buffer )
                if ( read >= 0 ) {
                    writer.write( buffer, 0, read )
                    copy
                }
            }

            copy
            content.close
            writer.flush
        }
    }

    /**
     * Uses an InputStream as a renderable
     */
    class StreamRenderer (
        private val content: InputStream
    ) extends Renderable {

        /** {@inheritDoc} */
        override def render ( output: OutputStream, codec: Codec ) = {
            val buffer = new Array[Byte](1024 * 4)

            def copy: Unit = {
                val read = content.read( buffer )
                if ( read >= 0 ) {
                    output.write( buffer, 0, read )
                    copy
                }
            }

            copy
            content.close
            output.flush
        }
    }

    implicit def apply ( content: String ): Renderable
        = new StringRenderer( content )

    implicit def apply ( content: StringBuilder ): Renderable
        = new AnyRenderer( content )

    implicit def apply ( content: StringBuffer ): Renderable
        = new AnyRenderer( content )

    implicit def apply ( callback: () => String ): Renderable
        = new CallbackRenderer( callback )

    implicit def apply ( callback: (OutputStream, Codec) => Unit ): Renderable
        = new CallbackRenderer( callback )

    implicit def apply ( callback: (PrintStream) => Unit ): Renderable
        = new CallbackRenderer( callback )

    implicit def apply ( content: NodeSeq ): Renderable
        = apply( () => content.toString )

    implicit def apply ( content: Reader ): Renderable
        = new ReaderRenderer( content )

    implicit def apply ( content: InputStream ): Renderable
        = new StreamRenderer( content )

    implicit def apply ( content: File ): Renderable
        = new StreamRenderer( new FileInputStream(content) )

}

