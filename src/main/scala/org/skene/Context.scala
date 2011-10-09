package org.skene

/**
 * The data associated with a request
 */
trait Context {

    /**
     * The requested URL
     */
    def url: URL

    /**
     * Any parameters associated with the request
     */
    def params: Map[String, String] = Map()

    /**
     * Returns a version of this context with new parameters added
     */
    def withParams ( newParams: Map[String, String] ): Context = {
        newParams.nonEmpty match {
            case true => new ParameterizedContext( this, params ++ newParams )
            case false => this
        }
    }

    def withParams( newParams: (String, String)* ): Context
        = withParams( newParams.toMap )
}

/**
 * A context that wraps another context and updates parts of it
 */
abstract class ContextDecorator
    ( private val inner: Context )
    extends Context
{

    /**
     * @see Context
     */
    override def url: URL = inner.url

    /**
     * @see Context
     */
    override def params: Map[String, String] = inner.params
}

/**
 * A context wrapper that adds parameters to an existing context
 */
class ParameterizedContext
    ( inner: Context, newParams: Map[String, String] )
    extends ContextDecorator( inner )
{
    /**
     * @see Context
     */
    override val params: Map[String, String] = newParams
}

