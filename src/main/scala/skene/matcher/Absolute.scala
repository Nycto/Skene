package com.roundeights.skene.matcher

import com.roundeights.skene.Request
import com.roundeights.skene.Matcher

/**
 * A matcher that always passes
 */
class Always (
    private val params: Map[String,String] = Map()
) extends Matcher {

    /** {@inheritDoc} */
    override def matches ( request: Request ) = Matcher.Result(true, params)

    /** {@inheritDoc} */
    override def toString () = "(Always)"
}

/**
 * A matcher that never passes
 */
class Never extends Matcher {

    /** {@inheritDoc} */
    override def matches ( request: Request ) = Matcher.Result(false)

    /** {@inheritDoc} */
    override def toString () = "(Never)"
}

