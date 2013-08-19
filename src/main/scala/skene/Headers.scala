package com.roundeights.skene

import scala.collection.convert.Wrappers.JEnumerationWrapper
import javax.servlet.http.HttpServletRequest
import scala.collection.immutable.TreeMap

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
    def get(key: String): Seq[String] = headers.get(key).getOrElse( Seq() )

    /** Returns a single value for a specific header */
    def apply(key: String): Option[String] = get(key).headOption

    /** {@inheritDoc} */
    override def iterator: Iterator[(String, Seq[String])] = headers.iterator

    /** Adds a header */
    def +: ( kv: (String, String) ): Headers = {
        val seq: Seq[String] =  kv._2 +: get(kv._1)
        new Headers( headers + ( kv._1 -> seq ) )
    }

    /** Tests whether a header exists */
    def contains(key: String): Boolean = headers.contains(key)
}

