package org.skene.context

import javax.servlet.http.{HttpServletRequest => ServletRequest}
import javax.servlet.http.{HttpServletResponse => ServletResponse}

import org.skene.URL
import org.skene.Context

/**
 * Provides access to the relevant parts of a Jetty Request
 */
class JettyRequest ( private val request: ServletRequest ) {

    /**
     * The requested URL
     */
    def url = new URL( request.getRequestURL.toString )
}

/**
 * Provides access to the relevant parts of a Jetty Response
 */
class JettyResponse ( private val response: ServletResponse ) {

}

/**
 * The specific context implementation for Jetty
 */
class JettyContext
    ( private val request: JettyRequest, private val response: JettyResponse )
    extends Context
{

    /**
     * A constructor for building a context directly from
     * Jetty servlet data
     */
    def this( request: ServletRequest, response: ServletResponse )
        = this( new JettyRequest(request), new JettyResponse(response) )

    /**
     * The requested URL
     */
    override lazy val url = request.url

}


