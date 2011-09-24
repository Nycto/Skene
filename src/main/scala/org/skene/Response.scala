package org.skene

import java.io.Writer
import java.lang.StringBuilder
import java.lang.StringBuffer
import scala.xml.NodeSeq

/**
 * The bundled response data for a request
 */
class Response ( val content: Renderable ) {

}

/**
 * Companion object...
 */
object Response {

    implicit def apply ( content: Renderable )
        = new Response( content = content )

    implicit def apply ( content: String )
        = new Response( content = Renderable(content) )

    implicit def apply ( content: StringBuilder )
        = new Response( content = Renderable(content) )

    implicit def apply ( content: StringBuffer )
        = new Response( content = Renderable(content) )

    implicit def apply ( thunk: () => String )
        = new Response( content = Renderable(thunk) )

    implicit def apply ( content: NodeSeq )
        = new Response( content = Renderable(content) )
}

