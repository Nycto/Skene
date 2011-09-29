package org.skene {

    import org.skene.matcher._
    import scala.collection.immutable.Map
    import scala.collection.immutable.HashMap

    /**
     * Helper methods creating matchers
     */
    object Matcher {

        /**
         * The results of a matcher pass
         */
        case class Result (
            val passed: Boolean,
            val params: Map[String, String] = HashMap()
        )

        def path ( path: String ) = new Path(path)
        val always = new Always
        val never = new Never
    }

    /**
     * A construct for matching patterns against a context
     */
    trait Matcher {
        /**
         * Returns whether a given context matches
         */
        def matches ( context: Context ): Matcher.Result
    }
}

package org.skene.matcher {

    import org.skene.Context
    import org.skene.Matcher

    /**
     * A matcher that always passes
     */
    class Always extends Matcher {
        /**
         * @see Matcher
         */
        override def matches ( context: Context ) = Matcher.Result(true)

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
        override def matches ( context: Context ) = Matcher.Result(false)

        /**
         * Create a readable description of this matcher
         */
        override def toString () = "[Never Matcher]"
    }
}

