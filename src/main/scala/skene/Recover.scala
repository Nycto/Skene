package com.roundeights.skene

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

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

    /**
     * Attempts to apply this exception and then rethrow it
     */
    def andRethrow ( err: Throwable ): Nothing = {
        if ( action.isDefinedAt(err) )
            action( err )
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
        def onSuccess ( callback: PartialFunction[A, Unit] ): OnSuccess[A] = {
            new OnSuccess(future.map {
                case value if callback.isDefinedAt(value) => {
                    try {
                        callback(value)
                        value
                    } catch {
                        case err: Throwable => andRethrow( err )
                    }
                }
                case value => value
            })
        }

        /** Applies a callback if the future was successful */
        def map[O] ( callback: A => O ): OnSuccess[O]
            = fromFuture( future.map( callback ) )

        /** Applies a callback if the future was successful */
        def flatMap[O] ( callback: A => Future[O] ): OnSuccess[O]
            = fromFuture( future.flatMap( callback ) )

        /** Applies a callback if the future was successful */
        def foreach[O] ( callback: A => O ): Unit
            = future.foreach(value => from { callback(value) })

        /** Filters this future according to a predicate */
        def withFilter ( predicate: A => Boolean ): OnSuccess[A] = {
            new OnSuccess( future.flatMap(value => {
                try {
                    val pass = predicate( value )
                    future.filter( _ => pass )
                }
                catch {
                    case err: Throwable => andRethrow( err )
                }
            } ))
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

    /** Returns a pattern matcher that will call this instance */
    def matchAll: PartialFunction[Throwable, Unit] = {
        case err: Throwable => orRethrow(err)
    }

    /** Returns a pattern matcher that will call this instance */
    def matcher (
        cases: PartialFunction[Throwable, Throwable]
    ): PartialFunction[Throwable, Unit] = {
        case err: Throwable => orRethrow(
            if ( cases.isDefinedAt(err) ) cases(err)
            else err
        )
    }

    /** Builds a new Recover instance from a callback */
    def using ( action: PartialFunction[Throwable, Unit] ): Recover
        = ( new Recover(action) ).orFallBackTo( this )
}

