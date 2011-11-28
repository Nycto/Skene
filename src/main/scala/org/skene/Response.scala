package org.skene

import java.io.Writer
import java.lang.StringBuilder
import java.lang.StringBuffer
import scala.xml.NodeSeq

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
     * A helper for building an HTML response
     */
    def html (
        content: Renderable = Renderable(""),
        code: ResponseCode = Response.OK
    ) = Response( content, code ).isHtml

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

    /**
     * The base class for the enumeration of header names
     */
    sealed abstract class Header ( val name: String )

    /**
     * The supported list of HTTP headers
     */
    object Header {
        implicit def headerToString ( header: Header ): String = header.name

        case class AcceptRanges() extends Header("Accept-Ranges")
        case class Age() extends Header("Age")
        case class Allow() extends Header("Allow")
        case class CacheControl() extends Header("Cache-Control")
        case class Connection() extends Header("Connection")
        case class ContentEncoding() extends Header("Content-Encoding")
        case class ContentLanguage() extends Header("Content-Language")
        case class ContentLength() extends Header("Content-Length")
        case class ContentLocation() extends Header("Content-Location")
        case class ContentMD5() extends Header("Content-MD5")
        case class ContentDisposition() extends Header("Content-Disposition")
        case class ContentRange() extends Header("Content-Range")
        case class ContentType() extends Header("Content-Type")
        case class Date() extends Header("Date")
        case class ETag() extends Header("ETag")
        case class Expires() extends Header("Expires")
        case class LastModified() extends Header("Last-Modified")
        case class Link() extends Header("Link")
        case class Location() extends Header("Location")
        case class P3P() extends Header("P3P")
        case class Pragma() extends Header("Pragma")
        case class ProxyAuthenticate() extends Header("Proxy-Authenticate")
        case class Refresh() extends Header("Refresh")
        case class RetryAfter() extends Header("Retry-After")
        case class Server() extends Header("Server")
        case class SetCookie() extends Header("Set-Cookie")
        case class StrictTransportSecurity() extends Header("Strict-Transport-Security")
        case class Trailer() extends Header("Trailer")
        case class TransferEncoding() extends Header("Transfer-Encoding")
        case class Vary() extends Header("Vary")
        case class Via() extends Header("Via")
        case class Warning() extends Header("Warning")
        case class WWWAuthenticate() extends Header("WWW-Authenticate")
    }

    /**
     * The base class for the enumeration of common Content-Type headers
     */
    sealed abstract class ContentType ( val mimeType: String )

    /**
     * The supported list of HTTP methods
     */
    object ContentType {
        implicit def contentTypeToString (
            mimeType: ContentType
        ): String = mimeType.mimeType

        case class Atom() extends ContentType("application/atom+xml")
        case class Bin() extends ContentType("application/octet-stream")
        case class Bmp() extends ContentType("image/bmp")
        case class Css() extends ContentType("text/css")
        case class Gif() extends ContentType("image/gif")
        case class Html() extends ContentType("text/html;charset=utf-8")
        case class Icon() extends ContentType("image/x-icon")
        case class Jpeg() extends ContentType("image/jpeg")
        case class Javascript() extends ContentType("application/x-javascript")
        case class Pdf() extends ContentType("application/pdf")
        case class Png() extends ContentType("image/png")
        case class Svg() extends ContentType("image/svg+xml")
        case class Swf() extends ContentType("application/x-shockwave-flash")
        case class Text() extends ContentType("text/plain")
        case class Tiff() extends ContentType("image/tiff")
        case class Xhtml() extends ContentType("application/xhtml+xml")
        case class Xml() extends ContentType("application/xml")
        case class Xslt() extends ContentType("application/xslt+xml")
        case class Zip() extends ContentType("application/zip")
    }

}

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
    val headers: Map[String,String] = Map()
) {

    /**
     * Creates a new copy of this response with the given header
     */
    def header( header: (String, String) ): Response
        = new Response( content, code, headers + header )

    /**
     * Creates a new copy of this response with the given header, but only
     * if it doesn't already exist
     */
    def addHeader( header: (String, String) ): Response
        = if ( headers.isDefinedAt( header._1 ) ) this else this.header(header)

    /**
     * Creates a new copy of this response with the given content type
     */
    def contentType ( value: String ): Response
        = header( Response.Header.ContentType(), value )

    /**
     * Creates a new copy of this response, but with the HTML content
     * type set.
     */
    def isHtml = contentType( Response.ContentType.Html() )

}

