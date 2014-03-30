package com.roundeights.skene

import scala.io.Source
import java.io.{InputStream, ByteArrayInputStream}
import java.net.InetAddress

/**
 * Request companion
 */
object Request {

    /**
     * The base class for the enumeration of HTTP methods
     */
    sealed abstract class Method ( override val toString: String )

    /**
     * The supported list of HTTP methods
     */
    object Method {

        // I didn't use Enumeration here for a few reasons:
        // - The UNKNOWN class would have been a pain to implement
        // - Custom string parsing was needed to convert everything to upper
        // - Pattern matching needed to be supported

        case class HEAD() extends Method("HEAD")
        case class GET() extends Method("GET")
        case class POST() extends Method("POST")
        case class PUT() extends Method("PUT")
        case class DELETE() extends Method("DELETE")
        case class TRACE() extends Method("TRACE")
        case class OPTIONS() extends Method("OPTIONS")
        case class CONNECT() extends Method("CONNECT")
        case class PATCH() extends Method("PATCH")
        case class UNKNOWN( method: String ) extends Method("UNKNOWN")

        def apply ( value: String ): Method = {
            if ( value == null ) {
                UNKNOWN("Undefined")
            }
            else {
                value.toUpperCase match {
                    case "HEAD" => HEAD()
                    case "GET" => GET()
                    case "POST" => POST()
                    case "PUT" => PUT()
                    case "DELETE" => DELETE()
                    case "TRACE" => TRACE()
                    case "OPTIONS" => OPTIONS()
                    case "CONNECT" => CONNECT()
                    case "PATCH" => PATCH()
                    case _ => UNKNOWN( value )
                }
            }
        }

        val values: List[Method] = List(
            HEAD(), GET(), POST(), PUT(), DELETE(),
            TRACE(), OPTIONS(), CONNECT(), PATCH()
        )

    }

    /**
     * A running incrementor to generate request IDs
     */
    private[skene] val ids = new java.util.concurrent.atomic.AtomicLong(1)
}

/**
 * The data associated with a request
 */
trait Request {

    /**
     * Returns the ID for this request
     */
    def requestID: Long

    /**
     * The requested URL
     */
    def url: URL

    /**
     * Any parameters associated with the request
     */
    def params: Map[String, String]

    /**
     * The HTTP Request method for this request. I.e. GET, POST, PUT, DELETE
     */
    def method: Request.Method

    /**
     * The query string from the URL
     */
    def queryString: Option[String]

    /**
     * Returns an input stream of the body of this request.
     */
    def bodyStream: InputStream

    /**
     * A map of request headers
     */
    def headers: Headers

    /**
     * A container of cookies in this request
     */
    def cookies: CookieJar

    /**
     * Returns whether this request was made over a secure channel, like https
     */
    def isSecure: Boolean

    /**
     * The IP address of the request
     */
    def ip: InetAddress

    /**
     * Returns a Source iterator over the body of this request.
     */
    def body: Source = Source.fromInputStream( bodyStream )

    /**
     * Returns the body of this request as a String
     */
    def bodyStr: String = body.mkString

    /**
     * Returns whether this request used the GET method
     */
    def isGet: Boolean = method == Request.Method.GET()

    /**
     * Returns whether this request used the POST method
     */
    def isPost: Boolean = method == Request.Method.POST()

    /**
     * Returns whether this request used the DELETE method
     */
    def isDelete: Boolean = method == Request.Method.DELETE()

    /**
     * Returns whether this request used the PUT method
     */
    def isPut: Boolean = method == Request.Method.PUT()

    /**
     * Returns a version of this request with new parameters added
     */
    def withParams ( newParams: Map[String, String] ): Request = {
        newParams.nonEmpty match {
            case true => new ParameterizedRequest( this, params ++ newParams )
            case false => this
        }
    }

    /**
     * Returns a version of this request with new parameters added
     */
    def withParams( newParams: (String, String)* ): Request
        = withParams( newParams.toMap )

    /**
     * Returns a list of GET query parameters
     */
    lazy val queryParams = QueryString( queryString.getOrElse("") )

    /**
     * Returns a list of POST body parameters
     */
    lazy val bodyParams = QueryString( body.mkString )

    /** {@inheritDoc} */
    override def toString: String
        = "[Request #%d %s %s]".format( requestID, method, url )

}

/**
 * The companion for the bare request object
 */
object BareRequest {
    def apply (
        url: URL = URL("http://www.example.com"),
        params: Map[String, String] = Map(),
        method: Request.Method = Request.Method.GET(),
        body: String = "",
        headers: Headers = Headers(),
        queryString: Option[String] = None,
        cookies: CookieJar = new CookieJar,
        isSecure: Boolean = false,
        ip: InetAddress = InetAddress.getByAddress(Array[Byte](127,0,0,1))
    ) = new BareRequest(
        url, params, method,
        new ByteArrayInputStream( body.getBytes ),
        headers, queryString, cookies, isSecure, ip
    )
}

/**
 * A request object that fills in any gaps with default values
 */
class BareRequest (
    override val url: URL,
    override val params: Map[String, String],
    override val method: Request.Method,
    override val bodyStream: InputStream,
    override val headers: Headers,
    override val queryString: Option[String],
    override val cookies: CookieJar,
    override val isSecure: Boolean,
    override val ip: InetAddress
) extends Request {

    /** {@inheritDoct} */
    override lazy val requestID = Request.ids.getAndIncrement
}

/**
 * A request that wraps another request and updates parts of it
 */
abstract class RequestDecorator
    ( private val inner: Request )
    extends Request
{
    /** {@inheritDoc} */
    override def requestID = inner.requestID

    /** {@inheritDoc} */
    override def url = inner.url

    /** {@inheritDoc} */
    override def params = inner.params

    /** {@inheritDoc} */
    override def method = inner.method

    /** {@inheritDoc} */
    override def bodyStream = inner.bodyStream

    /** {@inheritDoc} */
    override def headers = inner.headers

    /** {@inheritDoc} */
    override def queryString = inner.queryString

    /** {@inheritDoc} */
    override def cookies = inner.cookies

    /** {@inheritDoc} */
    override def isSecure = inner.isSecure

    /** {@inheritDoc} */
    override def ip = inner.ip
}

/**
 * A request wrapper that adds parameters to an existing request
 */
class ParameterizedRequest(
    inner: Request,
    override val params: Map[String, String]
) extends RequestDecorator( inner )

