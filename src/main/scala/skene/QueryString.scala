package com.roundeights.skene

import java.net.URLEncoder
import java.net.URLDecoder

/** Companion */
object QueryString {

    /** Create a query string from a list of tuples */
    def apply ( params: (String, String)* ): QueryString
        = new QueryString( params.toList )

    /** Parses a string */
    def apply ( queryString: String ): QueryString = {
        def decode( value: String ) = URLDecoder.decode( value, "UTF8" )
        queryString.split('&').foldRight( QueryString() )( (part, accum) => {
            part.split("=", 2) match {
                case Array(key) if key != ""
                    => (decode(key) -> "") :: accum
                case Array(key, value) if key != "" || value != ""
                    => (decode(key) -> decode(value)) :: accum
                case _ => accum
            }
        })
    }

}

/**
 * Represents a URL or POST query string
 */
case class QueryString (
    override val toList: List[(String, String)] = Nil
) extends Seq[(String, String)] {

    /** {@inheritDoc} */
    override def iterator = toList.iterator

    /** {@inheritDoc} */
    override def apply ( index: Int ) = toList(index)

    /** {@inheritDoc} */
    override def length = toList.length

    /** Returns the first parameter matching the given key */
    def apply ( key: String ): Option[String]
        = toList.find( _._1 == key ).map( _._2 )

    /** Adds a parameter */
    def prepend ( param: (String, String) ): QueryString
        = new QueryString(param :: toList)

    /** Optionally adds a parameter */
    def prepend ( param: Option[(String, String)] ): QueryString = param match {
        case Some(tuple) => new QueryString(tuple :: toList)
        case None => this
    }

    /** Adds a parameter */
    def :: ( param: (String, String) ): QueryString = prepend(param)

    /** Optionally adds a parameter */
    def :: ( param: Option[(String, String)] ): QueryString = prepend(param)

    /** {@inheritDoc} */
    override def toString: String = {
        toList.map( param => "%s=%s".format(
            URLEncoder.encode( param._1, "UTF8" ),
            URLEncoder.encode( param._2, "UTF8" )
        ) ).mkString("&")
    }

    /** Groups a parameter by key for parameters that appear multiple times */
    def grouped: Map[String, Seq[String]] = {
        toList.foldRight( Map[String, Seq[String]]() ) {
            (pair, accum) => accum + (
                pair._1 -> ( pair._2 +: accum.get(pair._1).getOrElse(Nil) )
            )
        }
    }

    /** Returns the keys for this query string */
    def keys: Set[String] = toList.foldLeft( Set[String]() )( _ + _._1 )

    /** Returns the values in this query string */
    def values: Set[String] = toList.foldLeft( Set[String]() )( _ + _._2 )
}

