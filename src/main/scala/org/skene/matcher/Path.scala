package org.skene.matcher

import org.skene.Context
import org.skene.Matcher


/**
 * The interface for a string of pattern matchers
 */
private trait PathScanner {
    def apply ( path: String ): Matcher.Result
}

/**
 * A terminating scanner that asserts that an entire path has been consomed
 * by any other scanners leading up to this one
 */
private case class TerminusScanner () extends PathScanner {
    override def apply ( path: String ) = Matcher.Result( path.isEmpty )
}

/**
 * A scanner that does a straight comparison of a path to a given
 */
private case class CompareScanner
    ( private val versus: String, private val next: PathScanner )
    extends PathScanner
{
    override def apply ( path: String ) = {
        path.startsWith( versus ) match {
            case true => next( path.drop( versus.length ) )
            case false => Matcher.Result(false)
        }
    }
}

/**
 * A scanner that ungreedily consumes any characters it can
 */
private case class GlobScanner
    ( private val until: Char, private val next: PathScanner )
    extends PathScanner
{
    override def apply ( path: String ) = next( path.dropWhile( _ != until ) )
}

/**
 * A scanner that assigns a name to a wildcard match
 */
private case class NamedScanner (
    private val name: String,
    private val until: Char,
    private val next: PathScanner
) extends PathScanner {
    override def apply ( path: String ) = next( path.dropWhile( _ != until ) )
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

        val name = pattern.substring( pos, nameEnd )

        val length = pattern.length

        // If the name goes to the end of the pattern, we only need
        // the glob scanner
        if ( nameEnd == length ) {
            NamedScanner( name, '/', parent )
        }
        else {
            NamedScanner(
                name,
                pattern( nameEnd ) ,
                CompareScanner( pattern.substring( nameEnd, length ), parent )
            )
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

                // trim any stars off the remaining pattern
                val remaining = pattern.take(
                    pattern.lastIndexWhere( !wildcards.contains(_), pos ) + 1
                )

                val wildcardScanner = pattern(pos) match {
                    case '*' => buildGlob( pos, pattern, parent )
                    case ':' => buildNamed( pos, pattern, parent )
                }

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
    override def toString () = "[Path Matcher: " + versus + "]"
}

