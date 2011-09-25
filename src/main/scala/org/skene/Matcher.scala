package org.skene {

    import org.skene.matcher._

    /**
     * Helper methods creating matchers
     */
    object Matcher {
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
        def matches ( context: Context ): Boolean
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
        override def matches ( context: Context ) = true

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
        override def matches ( context: Context ) = false

        /**
         * Create a readable description of this matcher
         */
        override def toString () = "[Never Matcher]"
    }
}

