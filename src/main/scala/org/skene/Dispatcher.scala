package org.skene

import org.skene.util.LinkedList

/**
 * Dispatches a request against a set of handlers based on matching rules
 *
 * This class is thread safe
 */
class Dispatcher extends Handler {

    /**
     * A pairing of a matcher and it's handler
     */
    private class Entry ( val matcher: Matcher, val handler: Handler )

    /**
     * The list of entries collected in this dispatcher
     */
    private val entries = new LinkedList[Entry]

    /**
     * The default handler to use when none of the matchers apply
     */
    private var default: Option[Handler] = None

    /**
     * The handler to use when nothing matches and there is no default
     */
    final lazy private val unresolvable = Handler({ (request) =>
        <h1>Unresolvable:
            <span>{request.url.path.getOrElse("/").toString}</span>
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
    override def handle( request: Request ): Response = {

        val matched = entries.find( entry => {
            entry.matcher.matches(request) match {
                case Matcher.Result(false, _) => None
                case Matcher.Result(true, params) => {
                    Some( (params, entry.handler) )
                }
            }
        })

        matched match {
            case Some( (params, handler) )
                => handler.handle( request.withParams(params) )
            case None => default.getOrElse( unresolvable ).handle( request )
        }
    }

}

