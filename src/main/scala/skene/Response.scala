package com.roundeights.skene

import scala.language.implicitConversions
import java.util.Date


/**
 * Companion object...
 */
object Response {

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
    sealed abstract class HeaderField ( val name: String )

    /**
     * The supported list of HTTP headers
     */
    object Header {
        case class AcceptRanges() extends HeaderField("Accept-Ranges")
        case class Age() extends HeaderField("Age")
        case class Allow() extends HeaderField("Allow")
        case class CacheControl() extends HeaderField("Cache-Control")
        case class Connection() extends HeaderField("Connection")
        case class ContentEncoding() extends HeaderField("Content-Encoding")
        case class ContentLanguage() extends HeaderField("Content-Language")
        case class ContentLength() extends HeaderField("Content-Length")
        case class ContentLocation() extends HeaderField("Content-Location")
        case class ContentMD5() extends HeaderField("Content-MD5")
        case class ContentDisposition()
            extends HeaderField("Content-Disposition")
        case class ContentRange() extends HeaderField("Content-Range")
        case class ContentType() extends HeaderField("Content-Type")
        case class Date() extends HeaderField("Date")
        case class ETag() extends HeaderField("ETag")
        case class Expires() extends HeaderField("Expires")
        case class LastModified() extends HeaderField("Last-Modified")
        case class Link() extends HeaderField("Link")
        case class Location() extends HeaderField("Location")
        case class P3P() extends HeaderField("P3P")
        case class Pragma() extends HeaderField("Pragma")
        case class ProxyAuthenticate()
            extends HeaderField("Proxy-Authenticate")
        case class Refresh() extends HeaderField("Refresh")
        case class RetryAfter() extends HeaderField("Retry-After")
        case class Server() extends HeaderField("Server")
        case class SetCookie() extends HeaderField("Set-Cookie")
        case class StrictTransportSecurity()
            extends HeaderField("Strict-Transport-Security")
        case class Trailer() extends HeaderField("Trailer")
        case class TransferEncoding() extends HeaderField("Transfer-Encoding")
        case class Vary() extends HeaderField("Vary")
        case class Via() extends HeaderField("Via")
        case class Warning() extends HeaderField("Warning")
        case class WWWAuthenticate() extends HeaderField("WWW-Authenticate")
    }

    /**
     * A Header Tuple with the name and value
     */
    case class Header( val field: HeaderField, val value: String )

    /**
     * The base class for the enumeration of common Content-Type headers
     */
    sealed abstract class ContentType ( val mimeType: String )

    /**
     * The supported list of HTTP methods
     */
    object ContentType {
        case class Atom() extends ContentType("application/atom+xml")
        case class Bin() extends ContentType("application/octet-stream")
        case class Bmp() extends ContentType("image/bmp")
        case class Css() extends ContentType("text/css")
        case class Gif() extends ContentType("image/gif")
        case class Html() extends ContentType("text/html;charset=utf-8")
        case class Icon() extends ContentType("image/x-icon")
        case class Jpeg() extends ContentType("image/jpeg")
        case class JavaScript() extends ContentType("application/x-javascript")
        case class Json() extends ContentType("application/json")
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

    /**
     * A flag to flush the response for actor based responses
     */
    case class Flush()

    /**
     * A finalization flag for actor based responses
     */
    case class Done()

}


/**
 * The bundled response data for a request
 */
trait Response {

    /**
     * Sets a header in this response
     */
    def header( header: Response.Header ): Response

    /**
     * Sets the status code in this response
     */
    def code ( code: Response.Code ): Response

    /**
     * Appens the given content to this response
     */
    def content ( content: Renderable ): Response

    /**
     * Sends a cookie
     */
    def cookie ( cookie: Cookie ): Response

    /**
     * Flushes the response
     */
    def flush (): Response

    /**
     * Finalizes this response
     */
    def done (): Response

    /**
     * Sets a header in this response
     */
    def header( field: Response.HeaderField, value: String ): Response
        = header( Response.Header( field, value ) )

    /**
     * Sets a date based header in this response
     */
    def header( field: Response.HeaderField, value: Date ): Response
        = header( Response.Header( field, Request.dateFormat.format(value) ) )

    /**
     * Sends a location header
     */
    def location ( value: String ): Response
        = header( Response.Header.Location(), value )

    /**
     * Sends the given content type
     */
    def contentType ( value: String ): Response
        = header( Response.Header.ContentType(), value )

    /**
     * Clones this response and sets the Content-Type to HTML
     */
    def isHtml: Response = contentType( Response.ContentType.Html().mimeType )

    /**
     * Clones this response and sets the Content-Type to XML
     */
    def isXml: Response = contentType( Response.ContentType.Xml().mimeType )

    /**
     * Clones this response and sets the Content-Type to JSON
     */
    def isJson: Response = contentType( Response.ContentType.Json().mimeType )

    /**
     * Clones this response and sets the Content-Type to Plain Text
     */
    def isText: Response = contentType( Response.ContentType.Text().mimeType )

    /**
     * A helper for building an HTML response
     */
    def html ( renderable: Renderable ): Response
        = content( renderable ).isHtml

    /**
     * A helper for building a text response
     */
    def text ( renderable: Renderable ): Response
        = content( renderable ).isText

    /**
     * A helper for building a json response
     */
    def json ( renderable: Renderable ): Response
        = content( renderable ).isJson

    /**
     * A helper for building an XML response
     */
    def xml ( renderable: Renderable ): Response
        = content( renderable ).isXml

    /**
     * Sends a 200 OK
     */
    def ok = code( Response.Code.OK() )

    /**
     * Sends a 201 OK
     */
    def created = code( Response.Code.Created() )

    /**
     * Sends a 400 Bad Requestcode
     */
    def badRequest = code( Response.Code.BadRequest() )

    /**
     * Sends a 401 unauthorized code
     */
    def unauthorized = code( Response.Code.Unauthorized() )

    /**
     * Sends a 404 Not Found code
     */
    def notFound = code( Response.Code.NotFound() )

    /**
     * Sends a 500 internal server error code
     */
    def serverError = code( Response.Code.InternalServerError() )

    /**
     * Sends a 409 Conflict response code
     */
    def conflict = code( Response.Code.Conflict() )

    /**
     * Redirects the client using a 302 Found response code
     */
    def found ( url: String ): Response
        = code( Response.Code.Found() ).location(url)

    /**
     * Redirects the client using a 301 Moved response code
     */
    def moved ( url: String ): Response
        = code( Response.Code.Moved() ).location(url)

}

