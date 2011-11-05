package org.skene.matcher

import org.skene.Matcher
import org.skene.Request

/**
 * Asserts that a set of matchers all pass
 */
class And ( private val matchers: List[Matcher] ) extends Matcher {

    /**
     * @see Matcher
     */
    override def matches ( request: Request ) = {

        // Recursively walks a list of matchers and joins their results,
        // short circuiting when one of the matchers fails
        def walk (
            result: Matcher.Result,
            remaining: List[Matcher]
        ): Matcher.Result = {
            remaining match {
                case Nil => result
                case head :: tail => head.matches(request) match {
                    case Matcher.Result(false, _) => Matcher.Result(false)
                    case current => walk( result + current, tail )
                }
            }
        }

        walk( Matcher.Result(true), matchers )
    }

    /**
     * Create a readable description of this matcher
     */
    override def toString () = "[And: " + matchers.mkString(", ") + "]"

}

/**
 * Asserts that any of a list of matchers pass
 */
class Or ( private val matchers: List[Matcher] ) extends Matcher {

    /**
     * @see Matcher
     */
    override def matches ( request: Request ) = {

        // Recursively walks a list of matchers, short circuiting when
        // one of them passes
        def walk ( remaining: List[Matcher] ): Matcher.Result = {
            remaining match {
                case Nil => Matcher.Result(false)
                case head :: tail => head.matches(request) match {
                    case Matcher.Result(false, _) => walk(tail)
                    case current => current
                }
            }
        }

        walk( matchers )
    }

    /**
     * Create a readable description of this matcher
     */
    override def toString () = "[Or: " + matchers.mkString(", ") + "]"

}

