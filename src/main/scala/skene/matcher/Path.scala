package com.roundeights.skene.matcher

import com.roundeights.skene.Request
import com.roundeights.skene.Matcher


/**
 * Collects the bundle of information required for the PathScanners
 * to do their work
 */
case class ScannerBundle(
    val path: String,
    val index: Int = 0,
    val params: Map[String, String] = Map()
) {

    /** Creates a new bundle based on this one with the given path. */
    def path ( path: String ) = ScannerBundle( path, index, params )

    /** Drops a number of characters from the left hand side of the path */
    def drop ( count: Int )
        = ScannerBundle( path.drop(count), index, params )

    /** Copies this bundle and increments the index. */
    def inc = ScannerBundle(path, index + 1, params)

    /** Adds a parameter to this bundle */
    def add ( param: (String, String) )
        = ScannerBundle(path, index, params + param )
}


/**
 * The interface for a string of pattern matchers
 */
private trait PathScanner {

    /**
     * Scans the given bundle and determines if it matches the configuration
     * of this path scanner
     */
    def apply ( bundle: ScannerBundle ): Matcher.Result

    /**
     * Scans the given path to determine if it matches this configuration.
     */
    def apply ( path: String ): Matcher.Result = apply( ScannerBundle(path) )
}

/**
 * A terminating scanner that asserts that an entire path has been consomed
 * by any other scanners leading up to this one
 */
private case class TerminusScanner () extends PathScanner {

    /** {@inheritDoc} */
    override def apply ( bundle: ScannerBundle ) = {
        bundle.path.isEmpty match {
            case true => Matcher.Result( true, bundle.params )
            case false => Matcher.Result( false )
        }
    }

    /** {@inheritDoc} */
    override def toString = "($)"
}

/**
 * A scanner that does a straight comparison of a path to a given
 */
private case class CompareScanner (
    private val versus: String,
    private val next: PathScanner
) extends PathScanner {

    /** {@inheritDoc} */
    override def apply ( bundle: ScannerBundle ) = {
        bundle.path.startsWith( versus ) match {
            case true => next( bundle.drop(versus.length) )
            case false => Matcher.Result(false)
        }
    }

    /** {@inheritDoc} */
    override def toString = "(" + versus + ") " + next
}

/**
 * A set of objects oriented towards scanning Globs out of a path.
 */
private object Glob {

    /**
     * A scanner that consumes any characters it can
     */
    abstract class Config {

        /** Returns the key to use for the given bundle info */
        def key ( bundle: ScannerBundle ): String

        /** Produces a readable version of this config */
        def toString ( until: Char ): String
    }

    /**
     * The config info for a named glob scanner
     */
    class Named ( val name: String ) extends Config {

        /** {@inheritDoc} */
        override def key ( bundle: ScannerBundle ) = name

        /** {@inheritDoc} */
        override def toString( until: Char )
            = "Glob :%s until '%s'".format(name, until)
    }

    /**
     * The config info an indexed glob
     */
    class Indexed extends Config {

        /** {@inheritDoc} */
        override def key ( bundle: ScannerBundle ) = bundle.index.toString

        /** {@inheritDoc} */
        override def toString( until: Char ) = "Glob until '%s'".format(until)
    }

    /**
     * A non-greedy glob path scanner
     */
    def Ungreedy (
        next: PathScanner,
        until: Char,
        key: String,
        bundle: ScannerBundle
    ): Matcher.Result = {

        val split = bundle.path.indexWhere( _ == until ) match {
            case -1 => bundle.path.length()
            case result => result
        }

        next(
            bundle
                .drop(split)
                .inc
                .add( key -> bundle.path.take(split) )
        )
    }

    /**
     * A greedy path scanner
     */
    def Greedy (
        next: PathScanner,
        until: Char,
        key: String,
        bundle: ScannerBundle
    ): Matcher.Result = {

        // A helper method for recursively checking various positions
        def scan ( from: Int ): Matcher.Result = {

            val result = next(
                bundle
                    .inc
                    .drop( from )
                    .add( key -> bundle.path.take(from) )
            )

            // If this position matches, return that it does. Otherwise,
            // search backwards for the next delimiter and check to see if
            // that one happens to solve this scanner
            result match {
                case Matcher.Result(true, _) => result
                case Matcher.Result(false, _) => {
                    bundle.path.lastIndexOf( until, from - 1 ) match {
                        case -1 => result
                        case nextPos => scan( nextPos )
                    }
                }
            }
        }

        scan( bundle.path.length() )
    }

    /**
     * A glob path scanner
     */
    class Scanner (
        private val until: Char,
        private val config: Glob.Config,
        greedy: Boolean,
        private val next: PathScanner
    ) extends PathScanner {

        /** The partially applied scan callback */
        private val scan = {
            if ( greedy ) Greedy(next, until, _:String, _:ScannerBundle)
            else Ungreedy(next, until, _:String, _:ScannerBundle)
        }

        /** {@inheritDoc} */
        override def apply ( bundle: ScannerBundle )
            = scan( config.key(bundle), bundle )

        /** {@inheritDoc} */
        override def toString = "(%s%s) %s".format(
            if ( greedy ) "Greedy " else "",
            config.toString(until),
            next
        )
    }
}

/**
 * Builder methods for constructing a PathScanner
 */
private object PathScanner {

    /** A helper method for building the wildcard portion of a scanner */
    private def buildGlob (
        pos: Int, pattern: String, parent: PathScanner, greedy: Boolean
    ): PathScanner = {

        val length = pattern.length

        // If the wildcard is at the end of the pattern, we don't
        // need a static string comparison scanner
        if ( pos + 1 == length ) {
            new Glob.Scanner( '/', new Glob.Indexed, greedy, parent )
        }
        else {
            new Glob.Scanner(
                pattern( pos + 1 ),
                new Glob.Indexed,
                greedy,
                CompareScanner(
                    pattern.takeRight( length - pos - 1 ),
                    parent
                )
            )
        }
    }

    /** A helper method for building a named wildcard scanner */
    private def buildNamed (
        pos: Int, pattern: String, parent: PathScanner, greedy: Boolean
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
            name match {
                case "" => parent
                case _ => new Glob.Scanner(
                    '/', new Glob.Named(name), greedy, parent
                )
            }
        }
        else {
            val compare = CompareScanner(
                pattern.substring( nameEnd, length ), parent
            )

            name match {
                case "" => compare
                case _ => new Glob.Scanner(
                    pattern( nameEnd ),
                    new Glob.Named(name),
                    greedy,
                    compare
                )
            }
        }
    }

    /**
     * Builds a chain of path scanners from a pattern
     *
     * This works by scanning the string from right to left and looking for
     * key characters. It then recursively constructs a chain of scanners
     */
    private def buildScanner (
        pattern: String, parent: PathScanner
    ): PathScanner = {

        val wildcards = Set('*', ':')

        pattern.lastIndexWhere( wildcards.contains(_) ) match {

            // If the pattern doesn't contain any wildcards
            case -1 => CompareScanner( pattern, parent )

            // If the pattern DOES contain wildcards, we need a specific scanner
            case pos => {

                // If the wildcard repeats, calculate the position that it
                // repeats to
                val repeatsTo = 1 + pattern.lastIndexWhere(
                    !wildcards.contains(_),
                    pos
                )

                // The greediness mode in which the scanner should be run. This
                // is determined by whether they repeated the wildcard
                val greedy = repeatsTo != pos

                // Divine the callback to use for generating the scanner
                // specified by the type of wildcard
                val scanner = pattern(pos) match {
                    case '*' => buildGlob(pos, pattern, parent, greedy)
                    case ':' => buildNamed(pos, pattern, parent, greedy)
                }

                // trim any repeated wildcards off the remaining pattern
                val remaining = pattern.take( repeatsTo )

                // Recurse to build scanners for the remaining bits of
                // the pattern
                buildScanner( remaining, scanner )
            }
        }
    }

    /** Builds a scanner from a string pattern */
    def apply ( pattern: String ): PathScanner
        = buildScanner( pattern, TerminusScanner() )
}

/**
 * Matches against the path of a request
 */
class Path ( path: String ) extends Matcher {

    /** The path being compared against */
    private val versus = if ( path.startsWith("/") ) path else "/" + path

    /** A list of matcher functions */
    private val scanner = PathScanner( versus )

    /** {@inheritDoc} */
    override def matches ( request: Request )
        = scanner( request.url.path.getOrElse("/") )

    /** {@inheritDoc} */
    override def toString () = "(Path " + scanner + ")"
}

