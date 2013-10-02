package com.roundeights.skene.request

import javax.servlet.http.HttpServletRequest

import scala.io.Source
import scala.collection.JavaConversions._

import com.roundeights.skene.{URL, Request, Cookie, CookieJar, Headers}

/**
 * The specific request implementation for Jetty
 */
class ServletRequest (
    private val request: HttpServletRequest
) extends Request {

    /** {@inheritDoct} */
    override lazy val requestID = Request.ids.getAndIncrement

    /** {@inheritDoct} */
    override lazy val url = new URL( request.getRequestURL.toString )

    /** {@inheritDoct} */
    override lazy val params = Map[String, String]()

    /** {@inheritDoct} */
    override lazy val method = Request.Method( request.getMethod )

    /** {@inheritDoct} */
    override def bodyStream = request.getInputStream

    /** {@inheritDoct} */
    override def queryString = request.getQueryString match {
        case null => None
        case "" => None
        case queryStr => Some(queryStr)
    }

    /** {@inheritDoct} */
    override lazy val headers = Headers(request)

    /** {@inheritDoct} */
    override lazy val cookies = request.getCookies match {
        case null => CookieJar()
        case Array() => CookieJar()
        case raw => CookieJar( raw.map( cookie => Cookie(cookie) ):_* )
    }

    /** {@inheritDoct} */
    override def isSecure = request.isSecure

}


