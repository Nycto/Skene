package com.roundeights.skene

import scala.annotation.tailrec
import com.roundeights.skene._
import scala.concurrent.ExecutionContext
import java.util.concurrent.atomic.AtomicReference


/**
 * A interface for fluently building a matcher
 */
trait Fluent {

    /** The execution context in which to run */
    implicit protected def context: ExecutionContext

    /** Associates a matcher with a handler */
    def when ( matcher: Matcher )( handler: Handler ): Unit

    /** Attaches a matcher to a handler */
    class Finalize ( private val matcher: Matcher ) {

        def apply ( callback: (Recover, Request, Response) => Unit ): Unit
            = when( matcher )( Handler(callback) )

        def apply ( callback: (Request, Response) => Unit ): Unit
            = when( matcher )( Handler(callback) )

        def apply ( callback: (Response) => Unit ): Unit
            = when( matcher )( Handler(callback) )

        def apply ( handler: Handler ): Unit
            = when( matcher )( handler )

        /**
         * Creates a fluent interface that will combine this matcher with
         * another expression
         */
        private def combiner(
            combine: (Matcher, Matcher) => Matcher
        ): Fluent = {
            val outerFluent = Fluent.this
            val outerCtx = Fluent.this.context
            val outerMatch = matcher
            new Fluent {
                override implicit protected val context = outerCtx
                override def when (matcher: Matcher)(handler: Handler): Unit
                    = outerFluent.when( combine(outerMatch, matcher) )(handler)
            }
        }

        /**
         * Builds a new Fluent matcher that requires this matcher and another
         * to both pass
         */
        def and ( other: Finalize ): Finalize
            = new Finalize( Matcher.and(matcher, other.matcher) )

        /**
         * Builds a new Fluent matcher that requires this matcher and another
         * to both pass
         */
        def and: Fluent = combiner( Matcher.and(_, _) )

        /**
         * Builds a new Fluent matcher that will match when either this
         * or another matcher pass
         */
        def or ( other: Finalize ): Finalize
            = new Finalize( Matcher.or(matcher, other.matcher) )

        /**
         * Builds a new Fluent matcher that will match when either this
         * or another matcher pass
         */
        def or: Fluent = combiner( Matcher.or(_, _) )
    }


    /** Adds a handler that matches all requests */
    def all: Finalize = new Finalize( Matcher.always )

    /** Adds a handler for any request matching the given path */
    def request ( path: String ): Finalize
        = new Finalize( Matcher.path(path) )

    /** A helper method for building method specific handlers */
    def method ( method: Request.Method ): Finalize
        = new Finalize( Matcher.method( method ) )

    /** Applies a handler for GET requests */
    lazy val isGet: Finalize = method( Request.Method.GET() )

    /** Applies a handler for POST requests */
    lazy val isPost: Finalize = method( Request.Method.POST() )

    /** Applies a handler for DELETE requests */
    lazy val isDelete: Finalize = method( Request.Method.DELETE() )

    /** Applies a handler for PUT requests */
    lazy val isPut: Finalize = method( Request.Method.PUT() )

    /** Applies a handler for PATCH requests */
    lazy val isPatch: Finalize = method( Request.Method.PATCH() )

    /** Adds a handler for GET requests to the given path */
    def get ( path: String ): Finalize = isGet and request(path)

    /** Adds a handler for POST requests to the given path */
    def post ( path: String ): Finalize = isPost and request(path)

    /** Adds a handler for DELETE requests to the given path */
    def delete ( path: String ): Finalize = isDelete and request(path)

    /** Adds a handler for PUT requests to the given path */
    def put ( path: String ): Finalize = isPut and request(path)

    /** Adds a handler for PATCH requests to the given path */
    def patch ( path: String ): Finalize = isPatch and request(path)

    /** Adds a handler that matches secure requests */
    lazy val isSecure: Finalize = new Finalize( Matcher.isSecure )

    /** Adds a handler that matches non-secure requests */
    lazy val notSecure: Finalize = new Finalize( Matcher.notSecure )

    /** Adds a handler when the request has the given subdomain */
    def subdomain( subdomain: String ): Finalize
        = new Finalize( Matcher.subdomain(subdomain) )

    /** Adds a handler when the request does not have a subdomain */
    lazy val noSubdomain: Finalize = new Finalize( Matcher.noSubdomain )

    /** Sets up a handler for the root directory */
    lazy val index: Finalize = request("/")

    /** Uses a custom callback as a matcher */
    def when ( callback: Request => Boolean ): Finalize = {
        new Finalize( Matcher.call { request =>
            Matcher.Result( callback(request) )
        } )
    }

    /** Uses a custom thunk as a matcher */
    def when ( callback: => Boolean ): Finalize
        = new Finalize( Matcher.call { Matcher.Result( callback ) } )
}


/**
 * A fluent interface for building Skene dispatchers
 */
class Skene (
    override implicit protected val context: ExecutionContext
) extends Handler with Matcher with Fluent {

    /** An interface for managing a Dispatcher in a thread safe way */
    private class LazyAtomicRef[T]
        ( create: => T )
        ( implicit ev: Null <:< T )
    {

        /** The internal reference */
        private val ref = new AtomicReference[T]

        /** Returns the current ref */
        @tailrec final def get: T = {
            val value = ref.get
            if ( value != null )
                value
            else if ( ref.compareAndSet(null, create) )
                create
            else
                get
        }

        /** Changes the value of the ref in a thread safe manner */
        @tailrec final def set ( callback: T => T ): Unit = {
            val current = get
            val changed = callback( current )
            if ( !ref.compareAndSet(current, changed) )
                set( callback )
        }
    }

    /** The internal dispatcher */
    private val dispatcher = new LazyAtomicRef[Dispatcher](new Dispatcher)

    /** {@inheritDoc} */
    override def handle(
        recover: Recover, request: Request, response: Response
    ): Unit = {
        dispatcher.get.handle( recover, request, response )
    }

    /** {@inheritDoc} */
    override def matches ( request: Request ): Matcher.Result
        = dispatcher.get.matches( request )

    /** Adds a new matcher */
    override def when ( matcher: Matcher )( handler: Handler ): Unit
        = dispatcher.set( _.add( matcher, handler ) )

    /** Sets up a default handler */
    def default ( handler: Handler ): Unit
        = dispatcher.set( _.default(handler) )

    /** Sets up a default handler from a callback */
    def default ( handler: (Request, Response) => Unit ): Unit
        = default( Handler(handler) )

    /** Sets up a default handler from a callback */
    def default ( handler: (Recover, Request, Response) => Unit ): Unit
        = default( Handler(handler) )

    /** Sets up the handler for when exceptions are thrown */
    def error ( handler: Dispatcher.OnError ): Unit
        = dispatcher.set( _.error( handler ) )

    /** Sets up the handler for when exceptions are thrown */
    def error ( handler: Dispatcher.SimpleOnError ): Unit
        = dispatcher.set( _.error( handler ) )

    /** Delegates to another object if it matches */
    def delegate ( to: Handler with Matcher ): Unit = when( to )( to )

}


