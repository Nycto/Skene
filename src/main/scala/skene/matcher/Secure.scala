package com.roundeights.skene.matcher

import com.roundeights.skene.Request
import com.roundeights.skene.Matcher

/**
 * Matches whether the request is over SSL
 */
class Secure ( private val secure: Boolean = true ) extends Matcher {

    /** {@inheritDoc} */
    override def matches ( request: Request )
        = Matcher.Result( request.isSecure == secure )

    /** {@inheritDoc} */
    override def toString = "(Secure " + secure + ")"
}


