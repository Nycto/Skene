package com.roundeights.skene.matcher

import com.roundeights.skene.Request
import com.roundeights.skene.Matcher

/**
 * Matches against the HTTP method of a request
 */
class Method ( val method: Request.Method ) extends Matcher {

    /** {@inheritDoc} */
    override def matches ( request: Request )
        = Matcher.Result( request.method == method )

    /** {@inheritDoc} */
    override def toString () = "(Method " + method + ")"
}

