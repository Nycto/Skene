package com.roundeights.skene

import com.roundeights.skene.util.LinkedList

/**
 * Dispatches a request against a set of handlers based on matching rules
 *
 * This class is thread safe
 */
class Dispatcher (
    override protected val logger: Logger = Logger.logger
) extends Handler {

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
     * The handler to use when an exception is thrown
     */
    private var error: Option[(Throwable, Request, Response) => Unit] = None

    /**
     * The handler to use when nothing matches and there is no default
     */
    final lazy private val unresolvable = Handler { (request, response) =>
        response.notFound.html(
            <html>
                <head><title>404 Not Found</title></head>
                <body>
                    <h1>404 Not Found</h1>
                    <p>
                        The requested resource could not be located:
                        <span>{request.url.path.getOrElse("/").toString}</span>
                    </p>
                </body>
            </html>
        ).done
    }

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
     * Changes the error handler for this dispatcher
     */
    def error (
        handler: (Throwable, Request, Response) => Unit
    ): Dispatcher = {
        error.synchronized {
            error = Some(handler)
        }
        this
    }

    /**
     * Checks the list of possible handlers and executes
     * the one that matches
     */
    override def handle ( request: Request, response: Response ): Unit = {

        val matched = entries.find( entry => {
            entry.matcher.matches(request) match {
                case Matcher.Result(false, _) => None
                case Matcher.Result(true, params) => {
                    Some( (params, entry.handler) )
                }
            }
        })

        try {
            matched match {
                case None =>
                    default.getOrElse(unresolvable).handle(request, response)

                case Some( (params, handler) ) =>
                    handler.handle( request.withParams(params), response )
            }
        }
        catch {
            case err: Throwable => error match {
                case Some(_) => {
                    logger.error( err )
                    error.get( err, request, response )
                }
                case None => { throw err }
            }
        }

    }

}

