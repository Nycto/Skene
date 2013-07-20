package com.roundeights.skene

import scala.annotation.tailrec
import com.roundeights.skene._
import scala.concurrent.ExecutionContext
import java.util.concurrent.atomic.AtomicReference

/**
 * A fluent interface for building Skene dispatchers
 */
class Skene (
    implicit context: ExecutionContext
) extends Handler with Matcher {

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
    override def matches ( request: Request ): Matcher.Result
        = dispatcher.get.matches( request )

    /** Adds a new matcher */
    def when ( matcher: Matcher )( handler: Handler ): Unit
        = dispatcher.set( _.add( matcher, handler ) )

    /**
     * A helper class for fluently building a dispatcher
     */
    class Fluent ( val matcher: Matcher ) {

        /** {@inheritDoc} */
        override def toString = "Fluent(%s)".format(matcher)

        def apply ( callback: (Recover, Request, Response) => Unit ): Unit
            = when( matcher )( Handler(callback) )

        def apply ( callback: (Request, Response) => Unit ): Unit
            = when( matcher )( Handler(callback) )

        def apply ( callback: (Response) => Unit ): Unit
            = when( matcher )( Handler(callback) )

        def apply ( handler: Handler ): Unit
            = when( matcher )( handler )

        /**
         * Builds a new Fluent matcher that requires this matcher and another
         * to both pass
         */
        def and ( other: Fluent ): Fluent
            = new Fluent( Matcher.and(matcher, other.matcher) )

        /**
         * Builds a new Fluent matcher that will match when either this
         * or another matcher pass
         */
        def or ( other: Fluent ): Fluent
            = new Fluent( Matcher.or(matcher, other.matcher) )
    }

    /** {@inheritDoc} */
    override def handle(
        recover: Recover, request: Request, response: Response
    ): Unit = {
        dispatcher.get.handle( recover, request, response )
    }

    /** Adds a handler that matches all requests */
    def all: Fluent = new Fluent( Matcher.always )

    /** Adds a handler for any request matching the given path */
    def request ( path: String ): Fluent
        = new Fluent( Matcher.path(path) )

    /** A helper method for building method specific handlers */
    def method ( method: Request.Method ): Fluent
        = new Fluent( Matcher.method( method ) )

    /** Applies a handler for GET requests */
    lazy val isGet: Fluent = method( Request.Method.GET() )

    /** Applies a handler for POST requests */
    lazy val isPost: Fluent = method( Request.Method.POST() )

    /** Applies a handler for DELETE requests */
    lazy val isDelete: Fluent = method( Request.Method.DELETE() )

    /** Applies a handler for PUT requests */
    lazy val isPut: Fluent = method( Request.Method.PUT() )

    /** Applies a handler for PATCH requests */
    lazy val isPatch: Fluent = method( Request.Method.PATCH() )

    /** Adds a handler for GET requests to the given path */
    def get ( path: String ): Fluent = isGet and request(path)

    /** Adds a handler for POST requests to the given path */
    def post ( path: String ): Fluent = isPost and request(path)

    /** Adds a handler for DELETE requests to the given path */
    def delete ( path: String ): Fluent = isDelete and request(path)

    /** Adds a handler for PUT requests to the given path */
    def put ( path: String ): Fluent = isPut and request(path)

    /** Adds a handler for PATCH requests to the given path */
    def patch ( path: String ): Fluent = isPatch and request(path)

    /** Adds a handler that matches secure requests */
    lazy val isSecure: Fluent = new Fluent( Matcher.isSecure )

    /** Adds a handler that matches non-secure requests */
    lazy val notSecure: Fluent = new Fluent( Matcher.notSecure )

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

    /** Sets up a handler for the root directory */
    lazy val index: Fluent = request("/")

    /** Uses a custom callback as a matcher */
    def when ( callback: Request => Boolean ): Fluent = {
        new Fluent( Matcher.call { request =>
            Matcher.Result( callback(request) )
        } )
    }

    /** Uses a custom thunk as a matcher */
    def when ( callback: => Boolean ): Fluent
        = new Fluent( Matcher.call { Matcher.Result( callback ) } )

    /** Delegates to another object if it matches */
    def delegate ( to: Handler with Matcher ): Unit = when( to )( to )

}


