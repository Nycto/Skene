package org.skene.matcher

import org.skene.Context
import org.skene.Matcher


/**
 * The interface for a string of pattern matchers
 */
private trait PathScanner {
    def apply (
        path: String, index: Int, params: Map[String, String]
    ): Matcher.Result

    def apply ( path: String ): Matcher.Result = apply( path, 0, Map() )
}

/**
 * A terminating scanner that asserts that an entire path has been consomed
 * by any other scanners leading up to this one
 */
private case class TerminusScanner () extends PathScanner {
    override def apply (
        path: String, index: Int, params: Map[String, String]
    ) = {
        path.isEmpty match {
            case true => Matcher.Result( true, params )
            case false => Matcher.Result( false )
        }
    }

    override def toString = "[Terminus]"
}

/**
 * A scanner that does a straight comparison of a path to a given
 */
private case class CompareScanner
    ( private val versus: String, private val next: PathScanner )
    extends PathScanner
{
    override def apply (
        path: String, index: Int, params: Map[String, String]
    ) = {
        path.startsWith( versus ) match {
            case true => next( path.drop( versus.length ), index, params )
            case false => Matcher.Result(false)
        }
    }

    override def toString = "[Compare = '" + versus + "'] -> " + next
}

/**
 * The base class for path scanners that add to the parameter list
 */
abstract private class ParamAddingScanner
    ( private val until: Char, private val next: PathScanner )
    extends PathScanner
{
    protected def getKey ( index: Int ): String

    override def apply (
        path: String, index: Int, params: Map[String, String]
    ) = {
        val split = path.indexWhere( _ == until ) match {
            case -1 => path.length()
            case result => result
        }

        next(
            path.drop( split ),
            index + 1,
            params + (getKey(index) -> path.take(split))
        )
    }
}

/**
 * A scanner that ungreedily consumes any characters it can
 */
private case class GlobScanner
    ( private val until: Char, private val next: PathScanner )
    extends ParamAddingScanner (until, next)
{
    protected def getKey ( index: Int ) = index.toString

    override def toString = "[Glob @" + until + "] -> " + next
}

/**
 * A scanner that assigns a name to a wildcard match
 */
private case class NamedScanner (
    private val name: String,
    private val until: Char,
    private val next: PathScanner
) extends ParamAddingScanner (until, next) {
    protected def getKey ( index: Int ) = name

    override def toString = "[Named Glob " + name + "@" + until + "] -> " + next
}

/**
 * Builder methods for constructing a PathScanner
 */
private object PathScanner {

    private val wildcards = List('*', ':')

    /**
     * A helper method for building the wildcard portion of a scanner
     */
    private def buildGlob (
        pos: Int, pattern: String, parent: PathScanner
    ): PathScanner = {

        val length = pattern.length

        // If the wildcard is at the end of the pattern, we don't
        // need a static string comparison scanner
        if ( pos + 1 == length ) {
            GlobScanner( '/', parent )
        }
        else {
            GlobScanner(
                pattern( pos + 1 ),
                CompareScanner(
                    pattern.takeRight( length - pos - 1 ),
                    parent
                )
            )
        }
    }

    /**
     * A helper method for building a named wildcard scanner
     */
    private def buildNamed (
        pos: Int, pattern: String, parent: PathScanner
    ): PathScanner = {

        // Search rightward from the wildcard position to find all the letters
        // that constitute the name of this glob
        val nameEnd = {
            pattern.indexWhere(
                (chr) => !Character.isLetterOrDigit(chr) && chr != '_',
                pos + 1
            ) match {
                case -1 => pattern.length
                case endPos => endPos
            }
        }

        val name = pattern.substring( pos + 1, nameEnd )

        val length = pattern.length

        // If the name goes to the end of the pattern, we only need
        // the glob scanner
        if ( nameEnd == length ) {
            if ( name == "" )
                parent
            else
                NamedScanner( name, '/', parent )
        }
        else {
            val compare = CompareScanner(
                pattern.substring( nameEnd, length ), parent
            )

            if ( name == "" )
                compare
            else
                NamedScanner( name, pattern( nameEnd ), compare )
        }
    }

    /**
     * A helper method that builds a chain of path scanners from a pattern
     *
     * This works by scanning the string from right to left and looking for
     * key characters. It then recursively constructs a chain of scanners
     */
    private def buildScanner (
        pattern: String, parent: PathScanner
    ): PathScanner = {

        pattern.lastIndexWhere( wildcards.contains(_) ) match {

            // If the pattern doesn't contain any wildcards
            case -1 => CompareScanner( pattern, parent )

            // If the pattern DOES contain wildcards, we need a specific scanner
            case pos => {

                val wildcardScanner = pattern(pos) match {
                    case '*' => buildGlob( pos, pattern, parent )
                    case ':' => buildNamed( pos, pattern, parent )
                }

                // trim any stars off the remaining pattern
                val remaining = pattern.take(
                    pattern.lastIndexWhere( !wildcards.contains(_), pos ) + 1
                )

                buildScanner( remaining, wildcardScanner )
            }
        }
    }

    /**
     * Builds a scanner from a string pattern
     */
    def apply ( pattern: String ): PathScanner
        = buildScanner( pattern, TerminusScanner() )
}

/**
 * Matches against the path of a request
 */
class Path ( path: String ) extends Matcher {

    /**
     * The path being compared against
     */
    private val versus = if ( path.startsWith("/") ) path else "/" + path

    /**
     * A list of matcher functions
     */
    private val scanner = PathScanner( versus )

    /**
     * @see Matcher
     */
    override def matches ( context: Context )
        = scanner( context.url.path.getOrElse("/") )

    /**
     * Create a readable description of this matcher
     */
    override def toString () = "[Path Matcher: " + scanner + "]"
}

