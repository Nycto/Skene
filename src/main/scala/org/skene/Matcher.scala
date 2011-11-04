package org.skene {

    import org.skene.matcher._
    import scala.collection.immutable.Map
    import scala.collection.immutable.HashMap

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
    }
}

package org.skene.matcher {

    import org.skene.Request
    import org.skene.Matcher

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
}

