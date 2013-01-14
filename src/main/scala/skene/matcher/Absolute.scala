package com.roundeights.skene.matcher

import com.roundeights.skene.Request
import com.roundeights.skene.Matcher

/**
 * A matcher that always passes
 */
class Always (
    private val params: Map[String,String] = Map()
) extends Matcher {

    /**
     * @see Matcher
     */
    override def matches ( request: Request ) = Matcher.Result(true, params)

    /**
     * Create a readable description of this matcher
     */
    override def toString () = "[Always Matcher]"
}

/**
 * A matcher that never passes
 */
class Never extends Matcher {
    /**
     * @see Matcher
     */
    override def matches ( request: Request ) = Matcher.Result(false)

    /**
     * Create a readable description of this matcher
     */
    override def toString () = "[Never Matcher]"
}

