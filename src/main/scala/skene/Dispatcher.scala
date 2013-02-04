package com.roundeights.skene

import com.roundeights.skene.util.LinkedList
import scala.concurrent.ExecutionContext
import scala.annotation.tailrec

/**
 * Companion
 */
object Dispatcher {

    /**
     * The type for error handlers
     */
    type OnError = (Request, Response) => PartialFunction[Throwable, Unit]

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

/**
 * Dispatches a request against a set of handlers based on matching rules
 *
 * This class is thread safe
 */
class Dispatcher (
    private val entries: List[(Matcher, Handler)] = Nil,
    private val default: Option[Handler] = None,
    private val onError: Option[Dispatcher.OnError] = None
)(
    implicit context: ExecutionContext
) extends Handler {

    /**
     * The list of matchers, ordered for faster parsing
     */
    private lazy val reversed = entries.reverse

    /**
     * Adds a matcher/handler pair to this Dispatcher
     */
    def add ( matcher: Matcher, handler: Handler )
        = new Dispatcher( (matcher -> handler) :: entries, default, onError )

    /**
     * Changes the default handler for this dispatcher
     */
    def default ( handler: Handler )
        = new Dispatcher( entries, Some(handler), onError )

    /**
     * Changes the error handler for this dispatcher
     */
    def error ( handler: Dispatcher.OnError ): Dispatcher
        = new Dispatcher( entries, default, Some(handler) )

    /** {@inheritDoc} */
    override def handle (
        recover: Recover, request: Request, response: Response
    ): Unit = {

        @tailrec def findMatch (
            remaining: List[(Matcher, Handler)]
        ): Option[(Map[String,String], Handler)] = remaining match {
            case Nil => None
            case head :: tail => {
                head._1.matches(request) match {
                    case Matcher.Result(true, params)
                        => Some( (params, head._2) )
                    case Matcher.Result(false, _) => findMatch( tail )
                }
            }
        }

        // If a specific error handler has been defined, use it. Otherwise,
        // use the default error recovery object
        val customRecover = onError match {
            case None => recover
            case Some(onError) =>
                Recover.using( onError( request, response ) )
                    .orFallBackTo( recover )
        }

        customRecover.from {
            findMatch( reversed ) match {
                case None => {
                    default
                        .getOrElse( Dispatcher.onUnmatched )
                        .handle( customRecover, request, response )
                }

                case Some( (params, handler) ) => handler.handle(
                    customRecover, request.withParams(params), response
                )
            }
        }

    }

}

