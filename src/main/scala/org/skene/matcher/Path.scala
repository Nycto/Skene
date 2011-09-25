package org.skene.matcher

import org.skene.Context
import org.skene.Matcher

/**
 * Matches against the path of a request
 */
class Path ( path: String ) extends Matcher {

    /**
     * The path being compared against
     */
    private val versus = if ( path.startsWith("/") ) path else "/" + path

    /**
     * @see Matcher
     */
    override def matches ( context: Context )
        = context.url.path.getOrElse("/") == versus

    /**
     * Create a readable description of this matcher
     */
    override def toString () = "[Path Matcher: " + versus + "]"
}

