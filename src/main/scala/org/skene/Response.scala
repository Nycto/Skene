package org.skene

import java.io.Writer
import java.lang.StringBuilder
import java.lang.StringBuffer
import scala.xml.NodeSeq

import scala.collection.immutable.Map
import scala.collection.immutable.TreeMap

/**
 * An HTTP Response code
 */
case class ResponseCode ( val code: Int ) {
    assert( code >= 100 )
    assert( code < 600 )
}

/**
 * The bundled response data for a request
 */
class Response (
    val content: Renderable = Renderable(""),
    val code: ResponseCode = Response.OK,
    val headers: Map[String,String] = new TreeMap
) {

    /**
     * Builds a new response object from this one, but including a new header
     */
    def setHeader ( header: String, value: String )
        = new Response( content, code, headers + ((header, value)) )
}

/**
 * Companion object...
 */
object Response {

    def apply (
        content: Renderable = Renderable(""),
        code: ResponseCode = Response.OK
    ) = new Response( content, code )

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

    /**
     * A predefined set of common response codes
     */
    lazy val OK = ResponseCode( 200 )
    lazy val Created = ResponseCode( 201 )
    lazy val Accepted = ResponseCode( 202 )
    lazy val NoContent = ResponseCode( 204 )
    lazy val Moved = ResponseCode( 301 )
    lazy val Found = ResponseCode( 302 )
    lazy val SeeOther = ResponseCode( 303 )
    lazy val NotModified = ResponseCode( 304 )
    lazy val TemporaryRedirect = ResponseCode( 307 )
    lazy val BadRequest = ResponseCode( 400 )
    lazy val Unauthorized = ResponseCode( 401 )
    lazy val Forbidden = ResponseCode( 403 )
    lazy val NotFound = ResponseCode( 404 )
    lazy val MethodNotAllowed = ResponseCode( 405 )
    lazy val NotAcceptable = ResponseCode( 406 )
    lazy val Conflict = ResponseCode( 409 )
    lazy val Gone = ResponseCode( 410 )
    lazy val PreconditionFailed = ResponseCode( 412 )
    lazy val EntityTooLarge = ResponseCode( 413 )
    lazy val UnsupportedMediaType = ResponseCode( 415 )
    lazy val ExpectationFailed = ResponseCode( 417 )
    lazy val InternalServerError = ResponseCode( 500 )
    lazy val NotImplemented = ResponseCode( 501 )
    lazy val BadGateway = ResponseCode( 502 )
    lazy val ServiceUnavailable = ResponseCode( 503 )
    lazy val GatewayTimeout = ResponseCode( 504 )
}

