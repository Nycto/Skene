package com.roundeights.skene

import scala.concurrent.ExecutionContext
import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicReference
import scala.annotation.tailrec

/**
 * Companion
 */
object Dispatcher {

    /** The type for error handlers */
    type OnError
        = (Recover, Request, Response) => PartialFunction[Throwable, Unit]

    /** The type for simple error handlers */
    type SimpleOnError
        = (Request, Response) => PartialFunction[Throwable, Unit]

    /**
     * The handler to use when nothing matches and there is no default
     */
    def onUnmatched( implicit context: ExecutionContext ): Handler = {
        Handler { (request, response) =>
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
    }

}

/** Runs a request against a list of matchers */
private class MatchFinder[T] ( initial: Traversable[(Matcher, T)] ) {

    /** The list of matchers, ordered for faster parsing */
    private val entries = new ConcurrentLinkedQueue[(Matcher, T)]
    initial.map( entries.add _ )

    /** Add a new value */
    def add ( entry: (Matcher, T) ): Unit = entries.add(entry)

    /** Finds the matching handler for a request */
    def find (request: Request): Option[(T, Matcher.Result)] = {
        val iterator = entries.iterator

        @tailrec def find: Option[(T, Matcher.Result)] = {
            if ( !iterator.hasNext ) {
                None
            }
            else {
                val next = iterator.next
                next._1.matches(request) match {
                    case result@Matcher.Result(true, _) =>
                        Some( (next._2 -> result) )
                    case Matcher.Result(false, _) => find
                }
            }
        }

        find
    }
}

/**
 * Dispatches a request against a set of handlers based on matching rules
 *
 * This class is thread safe
 */
class Dispatcher (
    entryList: Seq[(Matcher, Handler)] = Nil,
    defaultHandler: Option[Handler] = None,
    errorHandler: Option[Dispatcher.OnError] = None
)(
    implicit context: ExecutionContext
) extends Handler with Matcher {

    /** The list of matchers, ordered for faster parsing */
    private val entries = new MatchFinder[Handler](entryList)

    /** The default handler to invoke */
    private val default = new AtomicReference[Option[Handler]]( defaultHandler )

    /** The default handler to invoke */
    private val onError
        = new AtomicReference[Option[Dispatcher.OnError]]( errorHandler )

    /** {@inheritDoc} */
    override def matches ( request: Request ): Matcher.Result = {
        if ( default.get.isDefined ) {
            Matcher.Result(true)
        }
        else {
            entries.find( request ) match {
                case Some((_, matched@Matcher.Result(true, _))) => matched
                case _ => Matcher.Result(false)
            }
        }
    }

    /** Adds a matcher/handler pair to this Dispatcher */
    def add ( entry: (Matcher, Handler ) ): Dispatcher = {
        entries.add( entry )
        this
    }

    /** Adds a matcher/handler pair to this Dispatcher */
    def add ( matcher: Matcher, handler: Handler ): Dispatcher
        = add( (matcher -> handler) )

    /** Changes the default handler for this dispatcher */
    def default ( handler: Handler ): Dispatcher = {
        default.set( Some(handler) )
        this
    }

    /** Changes the error handler for this dispatcher */
    def error ( handler: Dispatcher.OnError ): Dispatcher = {
        onError.set( Some(handler) )
        this
    }

    /** Changes the error handler for this dispatcher */
    def error ( handler: Dispatcher.SimpleOnError ): Dispatcher
        = error( (_, req, resp) => handler(req, resp) )

    /** {@inheritDoc} */
    override def handle (
        recover: Recover, request: Request, response: Response
    ): Unit = {

        // If a specific error handler has been defined, use it. Otherwise,
        // use the default error recovery object
        val customRecover = onError.get match {
            case None => recover
            case Some(onError) =>
                Recover.using( onError( recover, request, response ) )
                    .orFallBackTo( recover )
        }

        customRecover.from {
            entries.find(request) match {
                case Some( (handler, Matcher.Result(true, params)) ) => {
                    handler.handle(
                        customRecover, request.withParams(params), response
                    )
                }
                case _ => {
                    default.get
                        .getOrElse( Dispatcher.onUnmatched )
                        .handle( customRecover, request, response )
                }
            }
        }

    }

}

