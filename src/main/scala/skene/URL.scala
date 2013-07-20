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

}

