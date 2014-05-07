package com.roundeights.skene

import scala.collection.convert.Wrappers.JEnumerationWrapper
import javax.servlet.http.HttpServletRequest
import scala.collection.immutable.TreeMap

import java.util.{Date, TimeZone}
import java.text.{DateFormat, SimpleDateFormat, ParseException}
import javax.xml.bind.DatatypeConverter

/** @see Headers */
object Headers {

    /** Orders values in a case insensitive way */
    private object CaseInsensitiveOrdering extends Ordering[String] {
        /** {@inheritDoc} */
        def compare(x: String, y: String): Int = x.compareToIgnoreCase(y)
    }

    /** Creates a new map builder */
    private def mapBuilder
        = TreeMap.newBuilder[String, Seq[String]](CaseInsensitiveOrdering)

    /** Creates a new instance */
    def apply (): Headers = new Headers

    /** Creates a new instance from an existing map */
    def apply ( map: Map[String, Seq[String]] ): Headers = {
        val builder = mapBuilder
        map.foreach( builder += _ )
        new Headers( builder.result )
    }

    /** Creates a new instance from a list of tuples */
    def apply ( values: (String, String)* ): Headers
        = values.foldRight( new Headers )( _ +: _ )

    /** Creates a new value from a Servlet request */
    def apply ( request: HttpServletRequest ): Headers = {
        val builder = mapBuilder

        JEnumerationWrapper(request.getHeaderNames).foreach(name => {
            val seq = JEnumerationWrapper( request.getHeaders(name) ).toSeq
            builder += ( name -> seq )
        })

        new Headers( builder.result )
    }

    /** The date format for headers */
    private[skene] val dateFormat = new ThreadLocal[DateFormat]() {
        override protected def initialValue(): DateFormat = {
            val format = new SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss zzz")
            format.setTimeZone( TimeZone.getTimeZone("GMT") )
            format
        }
    };

    /** Thrown when a header is invalid */
    class InvalidHeader( message: String ) extends Exception( message )

    /**
     * Parses the Accept header per
     * http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html
     */
    class Accept ( val raw: String ) {

        /** Splits a list of params into tuples */
        private def parseParams(
            parts: List[String]
        ): List[(String, String)] = {
            parts.foldRight(
                List[(String, String)]()
            )( (chunk, accum) => chunk.split("=", 2).toList match {
                case key :: Nil => ( key.trim -> "" ) :: accum
                case key :: value :: Nil if key.trim.length > 0
                    => ( key.trim -> value ) :: accum
                case _ => accum
            })
        }

        /** The list of content types and their parameters */
        val params: Map[String, List[(String, String)]] = {
            raw.split(",").foldLeft(
                Map[String, List[(String, String)]]()
            )( (accum, part) => {
                part.split(";").toList match {
                    case mime :: _ if mime.trim.length == 0 => accum
                    case mime :: tail =>
                        accum + ( mime.trim.toLowerCase -> parseParams(tail) )
                }
            })
        }

        /** Returns the list of mime types */
        lazy val contentTypes: Set[String] = params.keys.toSet

        /** Returns whether a content type is accepted */
        def apply ( contentType: String ): Boolean
            = contentTypes.contains( contentType.toLowerCase )

        /** {@inheritDoc} */
        override def toString = params.keys.toList.sorted.map( mime => {
            params(mime) match {
                case Nil => mime
                case pieces => "%s; %s".format(
                    mime,
                    pieces.map(pair => "%s=%s".format(pair._1, pair._2) )
                        .sorted
                        .mkString("; ")
                )
            }
        }).mkString(", ")
    }
}

/**
 * A map of headers
 */
class Headers private (
    val headers: TreeMap[String, Seq[String]]
) extends Iterable[(String, Seq[String])] {

    /** Creates a new instance */
    def this() = this( TreeMap()(Headers.CaseInsensitiveOrdering) )

    /** Returns a specific header */
    def get(header: String): Seq[String]
        = headers.get( header ).getOrElse( Seq() )

    /** Returns a single value for a specific header */
    def apply(header: String): Option[String] = get(header).headOption

    /** {@inheritDoc} */
    override def iterator: Iterator[(String, Seq[String])] = headers.iterator

    /** Adds a header */
    def +: ( kv: (String, String) ): Headers = {
        val seq: Seq[String] =  kv._2 +: get(kv._1)
        new Headers( headers + ( kv._1 -> seq ) )
    }

    /** Tests whether a header exists */
    def contains(header: String): Boolean = headers.contains(header)

    /** Returns a header as a date */
    def getDate( header: String ): Option[Date] = {
        apply( header ).flatMap( date => try {
            Some( Headers.dateFormat.get.parse( date ) )
        } catch {
            case _: ParseException => None
            case _: NumberFormatException => None
        })
    }

    /** Returns the content type of this request, if it was defined */
    def contentType: Option[String]
        = apply("Content-Type").map( _.takeWhile( _ != ';' ) )

    /** Returns the if-modified-since date from the request */
    def ifModifiedSince: Option[Date] = getDate("If-Modified-Since")

    /** Returns the authorization header from the request */
    def authorization: Option[String] = apply("Authorization")

    /** Returns the username and password for a basic auth header */
    def basicAuth: Option[(String, String)] = {
        authorization
            .filter( _.trim.toLowerCase.startsWith("basic ") )
            .map( header => {
                val clean = header.trim.drop("basic ".length).trim

                val decoded = DatatypeConverter.parseBase64Binary(clean)
                if ( decoded.length == 0 ) {
                    throw new Headers.InvalidHeader(
                        "Basic Auth credentials could not be decoded")
                }

                val parts = new String( decoded, "UTF-8" ).split(":", 2)
                if ( parts.length != 2 ) {
                    throw new Headers.InvalidHeader(
                        "Basic Auth credentials are missing a colon (:)")
                }

                parts(0) -> parts(1)
            })
    }

    /** Parsed interpretation of the Accept header */
    lazy val accept = new Headers.Accept( apply("Accept").getOrElse("*/*") )
}

