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
        code: Response.Code = Response.Code.OK()
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
        code: Response.Code = Response.Code.OK()
    ) = Response( content, code ).isHtml

    /**
     * An HTTP Response code
     */
    sealed abstract class Code ( val code: Int ) {
        assert( code >= 100 )
        assert( code < 600 )
    }

    /**
     * A predefined set of common response codes
     */
    object Code {
        case class OK() extends Code(200)
        case class Created() extends Code(201)
        case class Accepted() extends Code(202)
        case class NoContent() extends Code(204)
        case class Moved() extends Code(301)
        case class Found() extends Code(302)
        case class SeeOther() extends Code(303)
        case class NotModified() extends Code(304)
        case class TemporaryRedirect() extends Code(307)
        case class BadRequest() extends Code(400)
        case class Unauthorized() extends Code(401)
        case class Forbidden() extends Code(403)
        case class NotFound() extends Code(404)
        case class MethodNotAllowed() extends Code(405)
        case class NotAcceptable() extends Code(406)
        case class Conflict() extends Code(409)
        case class Gone() extends Code(410)
        case class PreconditionFailed() extends Code(412)
        case class EntityTooLarge() extends Code(413)
        case class UnsupportedMediaType() extends Code(415)
        case class ExpectationFailed() extends Code(417)
        case class InternalServerError() extends Code(500)
        case class NotImplemented() extends Code(501)
        case class BadGateway() extends Code(502)
        case class ServiceUnavailable() extends Code(503)
        case class GatewayTimeout() extends Code(504)
    }

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
        case class HTML() extends ContentType("text/html;charset=utf-8")
        case class Icon() extends ContentType("image/x-icon")
        case class Jpeg() extends ContentType("image/jpeg")
        case class Javascript() extends ContentType("application/x-javascript")
        case class JSON() extends ContentType("application/json")
        case class Pdf() extends ContentType("application/pdf")
        case class Png() extends ContentType("image/png")
        case class SVG() extends ContentType("image/svg+xml")
        case class Swf() extends ContentType("application/x-shockwave-flash")
        case class Text() extends ContentType("text/plain")
        case class Tiff() extends ContentType("image/tiff")
        case class XHTML() extends ContentType("application/xhtml+xml")
        case class XML() extends ContentType("application/xml")
        case class XSLT() extends ContentType("application/xslt+xml")
        case class Zip() extends ContentType("application/zip")
    }

}

/**
 * The bundled response data for a request
 */
class Response (
    val content: Renderable = Renderable(""),
    val code: Response.Code = Response.Code.OK(),
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
     * Clones this response and sets the Content-Type to HTML
     */
    def isHtml = contentType( Response.ContentType.HTML() )

    /**
     * Clones this response and sets the Content-Type to XML
     */
    def isXML = contentType( Response.ContentType.XML() )

    /**
     * Clones this response and sets the Content-Type to JSON
     */
    def isJSON = contentType( Response.ContentType.JSON() )

    /**
     * Clones this response and sets the Content-Type to Plain Text
     */
    def isText = contentType( Response.ContentType.Text() )

    /**
     * {@inheritDoc}
     */
    override def toString: String = {
        "[Response %s {%s}]".format(
            code.code,
            headers.map( (h) => h._1 + ": " + h._2 ).mkString(", ")
        )
    }

}

