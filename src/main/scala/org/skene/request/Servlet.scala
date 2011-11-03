package org.skene.request

import javax.servlet.http.HttpServletRequest

import org.skene.URL
import org.skene.Request

/**
 * The specific request implementation for Jetty
 */
class ServletRequest (
    private val request: HttpServletRequest
) extends Request {

    /**
     * The requested URL
     */
    override lazy val url = new URL( request.getRequestURL.toString )

    /**
     * The request method
     */
    override lazy val method = Request.Method( request.getMethod )

}


