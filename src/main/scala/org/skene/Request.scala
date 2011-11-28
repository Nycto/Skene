package org.skene

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

        def apply ( value: String ): Method = value.toUpperCase match {
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

        val values: List[Method] = List(
            HEAD(), GET(), POST(), PUT(), DELETE(),
            TRACE(), OPTIONS(), CONNECT(), PATCH()
        )

    }

}

/**
 * The data associated with a request
 */
trait Request {

    /**
     * The requested URL
     */
    def url: URL

    /**
     * Any parameters associated with the request
     */
    def params: Map[String, String] = Map()

    /**
     * The HTTP Request method for this request. I.e. GET, POST, PUT, DELETE
     */
    def method: Request.Method

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

}

/**
 * The companion for the bare request object
 */
object BareRequest {
    def apply (
        url: URL = URL("http://www.example.com"),
        params: Map[String, String] = Map(),
        method: Request.Method = Request.Method.GET()
    ) = new BareRequest( url, params, method )
}

/**
 * A request object that fills in any gaps with default values
 */
class BareRequest (
    override val url: URL,
    override val params: Map[String, String],
    override val method: Request.Method
) extends Request

/**
 * A request that wraps another request and updates parts of it
 */
abstract class RequestDecorator
    ( private val inner: Request )
    extends Request
{
    override def url = inner.url
    override def params = inner.params
    override def method = inner.method
}

/**
 * A request wrapper that adds parameters to an existing request
 */
class ParameterizedRequest
    ( inner: Request, newParams: Map[String, String] )
    extends RequestDecorator( inner )
{
    /**
     * @see Request
     */
    override val params: Map[String, String] = newParams
}

