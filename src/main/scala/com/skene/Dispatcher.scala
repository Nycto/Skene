package main.scala.com.skene

/**
 * A pairing of a matcher and it's handler
 */
private case class Entry( val matcher: Matcher, val handler: Handler );

/**
 * Dispatches a request against a set of handlers based on matching rules
 *
 * This class is immutable, so any mutating methods actually return a new
 * instance that contains the requested changes
 */
class Dispatcher private (
    private val entries: List[Entry],
    private val defaultHdlr: Option[Handler]
) extends Handler {

    /**
     * The primary constructor...
     */
    def this () = this( Nil, None )

    /**
     * To make iteration faster when handling requests, this holds a reversed
     * copy of the entries list
     */
    lazy private val revEntries = entries.reverse

    /**
     * The handler to use when nothing matches and there is no default
     */
    final lazy private val unresolvable = Handler({ (context) =>
        <h1>Unresolvable:
            <span>{context.url.path.getOrElse("/").toString}</span>
        </h1>
    })

    /**
     * Returns a new Dispatcher with the given handler added to it
     */
    def add ( matcher: Matcher, handler: Handler ): Dispatcher
        = new Dispatcher( new Entry(matcher, handler) :: entries, defaultHdlr )

    /**
     * Returns a new Dispatcher with the given the default handler
     */
    def default ( handler: Handler ): Dispatcher
        = new Dispatcher( entries, Some(handler) )

    /**
     * Checks the list of possible handlers and executes
     * the one that matches
     */
    override def handle( context: Context ): Renderable = {

        val matched = revEntries.find( _.matcher.matches(context) );

        val handler = matched match {
            case Some(entry) => entry.handler
            case None => defaultHdlr.getOrElse( unresolvable )
        }

        handler.handle( context )
    }

}

