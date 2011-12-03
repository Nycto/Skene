package org.skene.request

import javax.servlet.http.HttpServletRequest

import scala.io.Source

import org.skene.URL
import org.skene.Request

/**
 * The specific request implementation for Jetty
 */
class ServletRequest (
    private val request: HttpServletRequest
) extends Request {

    /** {@inheritDoct} */
    override lazy val url = new URL( request.getRequestURL.toString )

    /** {@inheritDoct} */
    override lazy val method = Request.Method( request.getMethod )

    /** {@inheritDoct} */
    override def bodyStream = request.getInputStream

}


