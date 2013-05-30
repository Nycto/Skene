package com.roundeights.skene

import com.roundeights.skene._
import scala.concurrent.ExecutionContext
import java.util.concurrent.atomic.AtomicReference

/**
 * A fluent interface for building Skene dispatchers
 */
abstract class Skene (
    implicit context: ExecutionContext
) extends Handler with Matcher {

    /**
     * The dispatcher to collect into
     */
    private val dispatcher = new AtomicReference( new Dispatcher )

    /** {@inheritDoc} */
    override def matches ( request: Request ): Matcher.Result
        = dispatcher.get.matches( request )

    /**
     * Adds a new matcher
     */
    def when ( matcher: Matcher )( handler: Handler ): Unit
        = dispatcher.set( dispatcher.get.add( matcher, handler ) )

    /**
     * A helper class for fluently building a dispatcher
     */
    class Fluent ( private val matcher: Matcher ) {

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
    ): Unit
        = dispatcher.get.handle( recover, request, response )

    /**
     * Adds a handler for any request matching the given path
     */
    def request ( path: String ): Fluent
        = new Fluent( Matcher.path(path) )

    /**
     * A helper method for building method specific handlers
     */
    def method ( method: Request.Method ): Fluent
        = new Fluent( Matcher.method( method ) )

    /**
     * Applies a handler for GET requests
     */
    lazy val isGet: Fluent = method( Request.Method.GET() )

    /**
     * Applies a handler for POST requests
     */
    lazy val isPost: Fluent = method( Request.Method.POST() )

    /**
     * Applies a handler for DELETE requests
     */
    lazy val isDelete: Fluent = method( Request.Method.DELETE() )

    /**
     * Applies a handler for DELETE requests
     */
    lazy val isPut: Fluent = method( Request.Method.PUT() )

    /**
     * Adds a handler for GET requests to the given path
     */
    def get ( path: String ): Fluent = isGet and request(path)

    /**
     * Adds a handler for POST requests to the given path
     */
    def post ( path: String ): Fluent = isPost and request(path)

    /**
     * Adds a handler for DELETE requests to the given path
     */
    def delete ( path: String ): Fluent = isDelete and request(path)

    /**
     * Adds a handler for PUT requests to the given path
     */
    def put ( path: String ): Fluent = isPut and request(path)

    /**
     * Sets up a default handler
     */
    def default ( handler: Handler ): Unit
        = dispatcher.set( dispatcher.get.default( handler ) )

    /**
     * Sets up a default handler from a callback
     */
    def default ( handler: (Request, Response) => Unit ): Unit
        = default( Handler(handler) )

    /**
     * Sets up the handler for when exceptions are thrown
     */
    def error (
        handler: (Request, Response) => PartialFunction[Throwable, Unit]
    ): Unit = dispatcher.set( dispatcher.get.error( handler ) )

    /**
     * Sets up a handler for the root directory
     */
    lazy val index: Fluent = request("/")

    /**
     * Uses a custom callback as a matcher
     */
    def when ( callback: Request => Boolean ): Fluent = {
        new Fluent( Matcher.call { request =>
            Matcher.Result( callback(request) )
        } )
    }

    /**
     * Uses a custom thunk as a matcher
     */
    def when ( callback: => Boolean ): Fluent
        = new Fluent( Matcher.call { Matcher.Result( callback ) } )

    /**
     * Delegates to another object if it matches
     */
    def delegate ( to: Handler with Matcher ): Unit = when( to )( to )

}


