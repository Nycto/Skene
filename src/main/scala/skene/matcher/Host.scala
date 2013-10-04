package com.roundeights.skene.matcher

import com.roundeights.skene.Request
import com.roundeights.skene.Matcher

/**
 * Matches the host of a URL
 */
class Host ( host: String ) extends Matcher {

    /** Normalizes a host name for comparison */
    private def normalize ( name: String ) = {
        val lower = name.toLowerCase
        if ( lower.startsWith("www.") ) lower.drop(4) else lower
    }

    /** The hormalized host name */
    private val hostname = normalize(host)

    /** {@inheritDoc} */
    override def matches ( request: Request )
        = Matcher.Result( normalize(request.url.host) == hostname )

    /** {@inheritDoc} */
    override def toString = "[Host: " + hostname + "]"
}


