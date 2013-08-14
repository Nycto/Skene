package com.roundeights.skene

import java.net.{URL => JavaURL}
import java.lang.StringBuffer

import scala.language.implicitConversions

/**
 * Static builders for the URL object
 */
object URL {
    def apply ( url: URL ) = url
    implicit def apply ( url: String ) = new URL( url )
    implicit def apply ( url: StringBuffer ) = new URL( url.toString )
    implicit def urlToString ( url: URL ) = url.toString
    def unapply ( url: URL ): Option[String] = Some(url.toString)

    /** Trims the leading and trailing slashes from a path */
    private[URL] def trimPath( path: String )
        = path.dropWhile(_ == '/').reverse.dropWhile(_ == '/').reverse
}

/**
 * A URL
 */
class URL private ( private val inner: JavaURL ) extends Equals {

    /** Constructs a URL from a string */
    def this ( url: String ) = this( new JavaURL(url) )

    /** {@inheritDoc} */
    override def toString = inner.toString

    /** {@inheritDoc} */
    override def hashCode = inner.hashCode

    /** {@inheritDoc} */
    override def equals(other: Any): Boolean = other match {
        case that: URL =>
            that.canEqual(this) &&
            inner.equals( that.inner )
        case _ => false
    }

    /** {@inheritDoc} */
    override def canEqual(other: Any): Boolean = other.isInstanceOf[URL]

    /** Returns the host of this URL */
    def host: String = inner.getHost

    /** Returns the path of this URL */
    def path: Option[String] = inner.getPath match {
        case "" => None
        case path => Some(path)
    }

    /** Replaces the path of this directory */
    def withPath ( path: String ) = {
        val query = if (inner.getQuery == null) "" else ("?" + inner.getQuery)
        new URL( new JavaURL(
            inner.getProtocol, inner.getHost, inner.getPort,
            "/" + path.dropWhile( _ == '/' ) + query
        ) )
    }

    /** Prefixes the path of this URL with a specific directory */
    def prefixPath ( prefix: String ) = {
        val trimmed = URL.trimPath( prefix )
        withPath( path match {
            case None => trimmed
            case Some(path) if path.dropWhile(_ == '/') == "" => trimmed
            case Some(path) => trimmed + "/" + path.dropWhile(_ == '/')
        })
    }

    /** Returns the subdomain of this URL */
    def subdomain: Option[String] = {
        host.split('.').dropRight(2).mkString(".") match {
            case "" => None
            case "www" => None
            case sub => Some( sub )
        }
    }

}

