package org.skene.matcher

import org.skene.Request
import org.skene.Matcher

/**
 * Matches against the HTTP method of a request
 */
class Method ( val method: Request.Method ) extends Matcher {

    /**
     * @see Matcher
     */
    override def matches ( request: Request )
        = Matcher.Result( request.method == method )

    /**
     * Create a readable description of this matcher
     */
    override def toString () = "[Method: " + method + "]"

}
