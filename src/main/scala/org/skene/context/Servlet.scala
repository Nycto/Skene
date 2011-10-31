package org.skene.context

import javax.servlet.http.{HttpServletRequest => ServletRequest}

import org.skene.URL
import org.skene.Context

/**
 * The specific context implementation for Jetty
 */
class ServletContext ( private val request: ServletRequest ) extends Context {

    /**
     * The requested URL
     */
    override lazy val url = new URL( request.getRequestURL.toString )

}


