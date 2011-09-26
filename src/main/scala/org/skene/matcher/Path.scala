package org.skene.matcher

import org.skene.Context
import org.skene.Matcher


/**
 * The interface for a string of pattern matchers
 */
private trait PathScanner {
    def apply ( path: String ): Boolean
}

/**
 * A terminating scanner that asserts that an entire path has been consomed
 * by any other scanners leading up to this one
 */
private case class TerminusScanner () extends PathScanner {
    override def apply ( path: String ) = path.isEmpty
}

/**
 * A scanner that does a straight comparison of a path to a given
 */
private case class CompareScanner
    ( private val versus: String, private val next: PathScanner )
    extends PathScanner
{
    override def apply ( path: String ): Boolean
        = path.startsWith( versus ) && next( path.drop( versus.length ) )
}

/**
 * A scanner that ungreedily consumes any characters it can
 */
private case class GlobScanner
    ( private val until: Char, private val next: PathScanner )
    extends PathScanner
{
    override def apply ( path: String ): Boolean
        = next( path.dropWhile( _ != until ) )
}

/**
 * Builder methods for constructing a PathScanner
 */
private object PathScanner {

    private val wildcards = List('*')

    /**
     * A helper method for building the wildcard portion of a scanner
     */
    private def buildWildcarded (
        pos: Int, pattern: String, parent: PathScanner
    ): PathScanner = {

        val length = pattern.length

        // trim any stars off the remaining pattern
        val remaining = {
            pattern
                .take( pos + 1 )
                .take( pattern.lastIndexWhere( !wildcards.contains(_) ) + 1 )
        }

        // If the wildcard is at the end of the pattern, we don't
        // need a static string comparison scanner
        val glob = {
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

        buildScanner( remaining, glob )
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
            case pos => buildWildcarded( pos, pattern, parent );
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
    override def matches ( context: Context ): Boolean
        = scanner( context.url.path.getOrElse("/") )

    /**
     * Create a readable description of this matcher
     */
    override def toString () = "[Path Matcher: " + versus + "]"
}

