package org.skene

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
     * Returns a version of this request with new parameters added
     */
    def withParams ( newParams: Map[String, String] ): Request = {
        newParams.nonEmpty match {
            case true => new ParameterizedRequest( this, params ++ newParams )
            case false => this
        }
    }

    def withParams( newParams: (String, String)* ): Request
        = withParams( newParams.toMap )
}

/**
 * The companion for the bare request object
 */
object BareRequest {
    def apply (
        url: URL = URL("http://www.example.com"),
        params: Map[String, String] = Map()
    ) = new BareRequest( url, params )
}

/**
 * A request object that fills in any gaps with default values
 */
class BareRequest ( _url: URL, _params: Map[String, String] ) extends Request {
    override val url = _url
    override val params = _params
}

/**
 * A request that wraps another request and updates parts of it
 */
abstract class RequestDecorator
    ( private val inner: Request )
    extends Request
{

    /**
     * @see Request
     */
    override def url: URL = inner.url

    /**
     * @see Request
     */
    override def params: Map[String, String] = inner.params
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

