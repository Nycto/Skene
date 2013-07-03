package com.roundeights.skene

import com.roundeights.skene.matcher._
import scala.concurrent.ExecutionContext

/**
 * Helper methods creating matchers
 */
object Matcher {

    /**
     * Result Companion
     */
    object Result {
        def apply ( passed: Boolean, params: (String, String)* )
            = new Result( passed, Map( params:_* ) )
    }

    /**
     * The results of a matcher pass
     */
    case class Result (
        val passed: Boolean,
        val params: Map[String, String] = Map()
    ) {

        /**
         * Combines this result with another one
         */
        def + ( other: Result ) = Result(
            passed && other.passed,
            params ++ other.params
        )
    }

    /**
     * Builds a matcher from a path
     */
    def path ( path: String ) = new Path(path)

    /**
     * A matcher that will always pass
     */
    val always = new Always
    def always ( params: (String, String)* ) = new Always( Map(params:_*) )

    /**
     * A matcher that will never pass
     */
    val never = new Never

    /**
     * A matcher passes when any nested matchers pass
     */
    def and ( matcher: Matcher* ) = new And( matcher.toList )

    /**
     * A matcher passes when one of a nested list of matchers pass
     */
    def or ( matcher: Matcher* ) = new Or( matcher.toList )

    /**
     * A matcher that looks at the HTTP Request method of a request
     */
    def method ( method: Request.Method ) = new Method( method )

    /**
     * A matcher that looks at whether a request is secure
     */
    def isSecure = new Secure( true )

    /**
     * A matcher that looks at whether a request is not secure
     */
    def notSecure = new Secure( false )

    /**
     * Builds a matcher from a callback
     */
    def call ( call: (Request) => Matcher.Result ) = new Matcher {
        override def matches ( request: Request ) = call(request)
    }

    /**
     * Builds a matcher from a thunk
     */
    def call ( call: => Matcher.Result ) = new Matcher {
        override def matches ( request: Request ) = call
    }
}

/**
 * A construct for matching patterns against a request
 */
trait Matcher {
    /**
     * Returns whether a given request matches
     */
    def matches ( request: Request ): Matcher.Result

    /** Joins this matcher with a Handler */
    def handle
        ( handler: Handler )
        ( implicit context: ExecutionContext )
    : Handler with Matcher = {
        val self = this
        new Handler with Matcher {
            /** {@inheritDoc} */
            override def matches ( request: Request ): Matcher.Result
                = self.matches( request )
            /** {@inheritDoc} */
            override def handle(
                recover: Recover, request: Request, response: Response
            ): Unit = {
                handler.handle( recover, request, response )
            }
            /** {@inheritDoc} */
            override def toString
                = "Matcher/Handler(%s, %s)".format(self, handler)
        }
    }

}

