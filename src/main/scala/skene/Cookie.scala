package com.roundeights.skene

import javax.servlet.http.{Cookie => JavaCookie}

/** @see Cookie */
object Cookie {

    /** Create a cookie from a Java Cookie */
    def apply ( cookie: JavaCookie ) = new Cookie(
        cookie.getName, cookie.getValue,
        if ( cookie.getMaxAge < 0 ) None else Some(cookie.getMaxAge),
        Option( cookie.getDomain ), Option( cookie.getPath ),
        cookie.getSecure, cookie.isHttpOnly
    )

}

/**
 * An HTTP Cookie
 */
case class Cookie (
    val name: String,
    val value: String,
    val ttl: Option[Int] = None,
    val domain: Option[String] = None,
    val path: Option[String] = None,
    val secure: Boolean = false,
    val httpOnly: Boolean = true
) {

    /** Returns this cookie as a Java Cookie */
    def toJavaCookie: JavaCookie = {
        val cookie = new JavaCookie( name, value )
        ttl match {
            case None => cookie.setMaxAge(-1)
            case Some(age) => cookie.setMaxAge(age)
        }
        domain.foreach { cookie.setDomain _ }
        path.foreach { cookie.setPath _ }
        cookie.setHttpOnly( httpOnly )
        cookie.setSecure( secure )
        cookie
    }

    /** Deletes this cookie */
    def delete = Cookie(name, value, Some(0), domain, path, secure, httpOnly)

    /** Modifies values of this cookie */
    def set (
        name: String = this.name,
        value: String = this.value,
        ttl: Option[Int] = this.ttl,
        domain: Option[String] = this.domain,
        path: Option[String] = this.path,
        secure: Boolean = this.secure,
        httpOnly: Boolean = this.httpOnly
    ) = Cookie( name, value, ttl, domain, path, secure, httpOnly )
}

/**
 * An interface for managing a collection of cookies
 */
case class CookieJar (
    private val cookies: Cookie*
) extends Iterable[Cookie] {

    /** {@inheritDoc} */
    override def iterator = cookies.iterator

    /** Returns all the cookies with the given key */
    def all( name: String ): Seq[Cookie] = cookies.filter( _.name == name )

    /** Returns the first cookie with the given name */
    def apply( name: String ): Option[Cookie] = all(name).lastOption
}

