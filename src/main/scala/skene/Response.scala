package com.roundeights.skene

import scala.language.implicitConversions
import java.util.Date


/**
 * Companion object...
 */
object Response {

    /**
     * A predefined set of common response codes
     */
    object Code extends Enumeration {

        /** Defines individual response codes */
        case class Code( val code: Int ) extends Value {
            assert( code >= 100 )
            assert( code < 600 )
            override def id = code
        }

        val OK = Code(200)
        val Created = Code(201)
        val Accepted = Code(202)
        val NoContent = Code(204)
        val Moved = Code(301)
        val Found = Code(302)
        val SeeOther = Code(303)
        val NotModified = Code(304)
        val TemporaryRedirect = Code(307)
        val BadRequest = Code(400)
        val Unauthorized = Code(401)
        val Forbidden = Code(403)
        val NotFound = Code(404)
        val MethodNotAllowed = Code(405)
        val NotAcceptable = Code(406)
        val Conflict = Code(409)
        val Gone = Code(410)
        val PreconditionFailed = Code(412)
        val EntityTooLarge = Code(413)
        val UnsupportedMediaType = Code(415)
        val ExpectationFailed = Code(417)
        val InternalServerError = Code(500)
        val NotImplemented = Code(501)
        val BadGateway = Code(502)
        val ServiceUnavailable = Code(503)
        val GatewayTimeout = Code(504)
    }

    /** HTTP Response Codes */
    type Code = Code.Code

    /**
     * The supported list of HTTP headers
     */
    object Header extends Enumeration {
        val AcceptRanges = Value("Accept-Ranges")
        val Age = Value("Age")
        val Allow = Value("Allow")
        val CacheControl = Value("Cache-Control")
        val Connection = Value("Connection")
        val ContentEncoding = Value("Content-Encoding")
        val ContentLanguage = Value("Content-Language")
        val ContentLength = Value("Content-Length")
        val ContentLocation = Value("Content-Location")
        val ContentMD5 = Value("Content-MD5")
        val ContentDisposition = Value("Content-Disposition")
        val ContentRange = Value("Content-Range")
        val ContentType = Value("Content-Type")
        val Date = Value("Date")
        val ETag = Value("ETag")
        val Expires = Value("Expires")
        val LastModified = Value("Last-Modified")
        val Link = Value("Link")
        val Location = Value("Location")
        val P3P = Value("P3P")
        val Pragma = Value("Pragma")
        val ProxyAuthenticate = Value("Proxy-Authenticate")
        val Refresh = Value("Refresh")
        val RetryAfter = Value("Retry-After")
        val Server = Value("Server")
        val SetCookie = Value("Set-Cookie")
        val StrictTransportSecurity = Value("Strict-Transport-Security")
        val Trailer = Value("Trailer")
        val TransferEncoding = Value("Transfer-Encoding")
        val Vary = Value("Vary")
        val Via = Value("Via")
        val Warning = Value("Warning")
        val WWWAuthenticate = Value("WWW-Authenticate")
    }

    /** An HTTP Header Field */
    type HeaderField = Header.Value

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
    def header( field: Response.HeaderField, value: Date ): Response = header(
        Response.Header( field, Headers.dateFormat.get.format(value) )
    )

    /**
     * Sends a location header
     */
    def location ( value: String ): Response
        = header( Response.Header.Location, value )

    /**
     * Sends the given content type
     */
    def contentType ( value: String ): Response
        = header( Response.Header.ContentType, value )

    /**
     * Sends the given content type
     */
    def contentType ( value: Response.ContentType ): Response
        = contentType( value.mimeType )

    /**
     * Clones this response and sets the Content-Type to HTML
     */
    def isHtml: Response = contentType( Response.ContentType.Html() )

    /**
     * Clones this response and sets the Content-Type to XML
     */
    def isXml: Response = contentType( Response.ContentType.Xml() )

    /**
     * Clones this response and sets the Content-Type to JSON
     */
    def isJson: Response = contentType( Response.ContentType.Json() )

    /**
     * Clones this response and sets the Content-Type to Plain Text
     */
    def isText: Response = contentType( Response.ContentType.Text() )

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
    def ok = code( Response.Code.OK )

    /**
     * Sends a 201 OK
     */
    def created = code( Response.Code.Created )

    /**
     * Sends a 400 Bad Requestcode
     */
    def badRequest = code( Response.Code.BadRequest )

    /**
     * Sends a 401 unauthorized code
     */
    def unauthorized = code( Response.Code.Unauthorized )

    /**
     * Sends a 403 forbidden code
     */
    def forbidden = code( Response.Code.Forbidden )

    /**
     * Sends a 404 Not Found code
     */
    def notFound = code( Response.Code.NotFound )

    /**
     * Sends a 500 internal server error code
     */
    def serverError = code( Response.Code.InternalServerError )

    /**
     * Sends a 409 Conflict response code
     */
    def conflict = code( Response.Code.Conflict )

    /**
     * Redirects the client using a 302 Found response code
     */
    def found ( url: String ): Response
        = code( Response.Code.Found ).location(url)

    /**
     * Redirects the client using a 301 Moved response code
     */
    def moved ( url: String ): Response
        = code( Response.Code.Moved ).location(url)
}

