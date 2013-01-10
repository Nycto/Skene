package org.skene

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
class URL ( url: String ) {

    /**
     * The internal URL handler being scala-ized
     */
    private val inner = new JavaURL(url);

    /**
     * Converts this URL to a string
     */
    override def toString = inner.toString

    /**
     * Returns an integer hash of this URL
     */
    override def hashCode = inner.hashCode

    /**
     * Determines object equality
     */
    override def equals(other: Any): Boolean = other match {
        case that: URL =>
            that.canEqual(this) &&
            inner.equals( that.inner )
        case _ => false
    }

    /**
     * A helper method for determining equality
     */
    def canEqual(other: Any): Boolean = other.isInstanceOf[URL]

    /**
     * Returns the host of this URL
     */
    def host: String = inner.getHost

    /**
     * Returns the path of this URL
     */
    def path: Option[String] = inner.getPath match {
        case "" => None
        case path => Some(path)
    }

}

