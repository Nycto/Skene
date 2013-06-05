package com.roundeights.skene

import scala.concurrent.{ExecutionContext, Future}

/**
 * Recover companion
 */
object Recover {

    /** Builds a new Recover instance from a callback */
    def using ( action: PartialFunction[Throwable, Unit] ): Recover
        = new Recover( action )

    /** Recovers from an error with a logger */
    def errorAndLog ( resp: Response, log: Logger ): Recover = using {
        case err: Throwable => {
            log.error( err )
            resp.serverError.html(
                <html>
                    <head><title>500 Internal Server Error</title></head>
                    <body><h1>500 Internal Server Error</h1></body>
                </html>
            ).done
        }
    }

}

/**
 * An interface for recovering from errors
 */
class Recover ( private val action: PartialFunction[Throwable, Unit] ) {

    /**
     * Attempts to apply this exception or rethrows if the exception isn't
     * defined for this instance
     */
    def orRethrow ( err: Throwable ): Unit = {
        if ( action.isDefinedAt(err) )
            action( err )
        else
            throw err
    }

    /** Executes the given thunk and recovers if it throws an exception */
    def from ( thunk: => Unit ): Unit = try {
        thunk
    } catch {
        case err: Throwable => orRethrow( err )
    }

    /** Executes a partial function when a future is successful */
    class OnSuccess[A]
        ( val future: Future[A] )
        ( implicit context: ExecutionContext )
    {
        /** Executes a partial function on success */
        def onSuccess ( callback: PartialFunction[A, Unit] ): Unit = {
            future.onSuccess {
                case value if callback.isDefinedAt(value) => from {
                    callback( value )
                }
            }
        }
    }

    /** Executes the recovery code when a future fails */
    def fromFuture[A]
        ( future: Future[A] )
        ( implicit context: ExecutionContext )
    : OnSuccess[A] = {
        future.onFailure {
            case err: Throwable if action.isDefinedAt(err) => action(err)
        }
        new OnSuccess( future )
    }

    /** Executes the given thunk asyncrhonously and recovers from any errors */
    def async ( thunk: => Unit )( implicit context: ExecutionContext ): Unit
        = context.execute( new Runnable {
            override def run = from { thunk }
        } )

    /**
     * Attempts to handle an exception with this instance, but falls back
     * to the given Recover instance if the exception can't be handled by
     * this one.
     */
    def orFallBackTo ( recover: Recover ): Recover = Recover.using {
        case err: Throwable if action.isDefinedAt(err)
            => recover.from { action( err ) }
        case err: Throwable => recover.orRethrow( err )
    }

}

