package com.roundeights.skene.request

import javax.servlet.http.HttpServletRequest

import scala.io.Source
import scala.collection.JavaConversions._

import com.roundeights.skene.URL
import com.roundeights.skene.Request

/**
 * The specific request implementation for Jetty
 */
class ServletRequest (
    private val request: HttpServletRequest
) extends Request {

    /** {@inheritDoct} */
    override lazy val url = new URL( request.getRequestURL.toString )

    /** {@inheritDoct} */
    override lazy val params = Map[String, String]()

    /** {@inheritDoct} */
    override lazy val method = Request.Method( request.getMethod )

    /** {@inheritDoct} */
    override def bodyStream = request.getInputStream

    /** {@inheritDoct} */
    override lazy val headers: Map[String, String] = {
        request.getHeaderNames.foldLeft( Map[String, String]() ) {
            (map, key) => map + (key -> request.getHeader(key))
        }
    }

}


