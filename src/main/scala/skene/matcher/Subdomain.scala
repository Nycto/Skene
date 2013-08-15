package com.roundeights.skene.matcher

import com.roundeights.skene.Request
import com.roundeights.skene.Matcher

/**
 * Matches the subdomain of a URL
 */
class Subdomain ( subdomain: Option[String] ) extends Matcher {

    /** Normalized subdomain */
    private val sub: Option[String] = subdomain.flatMap( _.toLowerCase match {
        case "" => None
        case "www" => None
        case sub => Some(sub)
    } )

    /** {@inheritDoc} */
    override def matches ( request: Request )
        = Matcher.Result( request.url.subdomain.map( _.toLowerCase ) == sub )

    /** {@inheritDoc} */
    override def toString = "[Subdomain: " + sub + "]"
}


