package org.skene

import java.util.concurrent.ConcurrentLinkedQueue
import scala.collection.JavaConversions._

/**
 * Dispatches a request against a set of handlers based on matching rules
 *
 * This class is thread safe
 */
class Dispatcher extends Handler {

    /**
     * A pairing of a matcher and it's handler
     */
    private case class Entry( val matcher: Matcher, val handler: Handler )

    /**
     * The list of entries collected in this dispatcher
     */
    private val entries = new ConcurrentLinkedQueue[Entry]

    /**
     * The default handler to use when none of the matchers apply
     */
    private var default: Option[Handler] = None

    /**
     * The handler to use when nothing matches and there is no default
     */
    final lazy private val unresolvable = Handler({ (context) =>
        <h1>Unresolvable:
            <span>{context.url.path.getOrElse("/").toString}</span>
        </h1>
    })

    /**
     * Adds a matcher/handler pair to this Dispatcher
     */
    def add ( matcher: Matcher, handler: Handler ): Dispatcher = {
        entries.add( new Entry(matcher, handler) )
        this
    }

    /**
     * Changes the default handler for this dispatcher
     */
    def default ( handler: Handler ): Dispatcher = {
        default.synchronized {
            default = Some(handler)
        }
        this
    }

    /**
     * Checks the list of possible handlers and executes
     * the one that matches
     */
    override def handle( context: Context ): Renderable = {

        val matched = entries.iterator.find( _.matcher.matches(context) )

        val handler = matched match {
            case Some(entry) => entry.handler
            case None => default.getOrElse( unresolvable )
        }

        handler.handle( context )
    }

}

