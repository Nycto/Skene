package com.roundeights.skene

import com.roundeights.skene._
import com.roundeights.skene.Request.Method
import scala.concurrent.ExecutionContext


/** Attaches a matcher to a handler */
class Finalize (
    private[skene] val matcher: Matcher,
    private val done:( Matcher, Handler ) => Unit
) (
    implicit private val ctx: ExecutionContext
) {

    /** Joins this matcher with a Handler */
    def apply ( handler: Handler ): Unit = done( matcher, handler )

    /** Joins this matcher with a Handler */
    def apply ( callback: (Recover, Request, Response) => Unit ): Unit
        = apply( Handler(callback) )

    /** Joins this matcher with a Handler */
    def apply ( callback: (Request, Response) => Unit ): Unit
        = apply( Handler(callback) )

    /** Joins this matcher with a Handler */
    def apply ( callback: (Response) => Unit ): Unit
        = apply( Handler(callback) )

    /** {@inheritDoc} */
    override def toString = matcher.toString

    /**
     * Creates a fluent interface that will combine this matcher with
     * another expression
     */
    private def combiner(
        combine: (Matcher, Matcher) => Matcher
    ): Fluent = {
        new Fluent {
            override implicit protected val context = ctx
            override def when ( rightMatcher: Matcher ): Finalize
                = new Finalize( combine(matcher, rightMatcher), done )(ctx)
        }
    }

    /**
     * Builds a new Fluent matcher that requires this matcher and another
     * to both pass
     */
    def and ( other: Finalize ): Finalize
        = new Finalize( Matcher.and(matcher, other.matcher), done )

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
        = new Finalize(Matcher.or(matcher, other.matcher), done)

    /**
     * Builds a new Fluent matcher that will match when either this
     * or another matcher pass
     */
    def or: Fluent = combiner( Matcher.or(_, _) )
}

/**
 * A interface for fluently building a matcher
 */
trait Fluent {

    /** The execution context in which to run */
    implicit protected def context: ExecutionContext

    /** Associates a matcher with a handler */
    def when ( matcher: Matcher ): Finalize


    /** Adds a handler that matches all requests */
    def all: Finalize = when( Matcher.always )

    /** Adds a handler for any request matching the given path */
    def request ( path: String ): Finalize
        = when( Matcher.path(path) )

    /** A helper method for building method specific handlers */
    def method ( method: Method ): Finalize
        = when( Matcher.method( method ) )

    /** Applies a handler for GET requests */
    lazy val isGet: Finalize = method( Method.GET )

    /** Applies a handler for POST requests */
    lazy val isPost: Finalize = method( Method.POST )

    /** Applies a handler for DELETE requests */
    lazy val isDelete: Finalize = method( Method.DELETE )

    /** Applies a handler for PUT requests */
    lazy val isPut: Finalize = method( Method.PUT )

    /** Applies a handler for PATCH requests */
    lazy val isPatch: Finalize = method( Method.PATCH )

    /** Applies a handler for HEAD requests */
    lazy val isHead: Finalize = method( Method.HEAD )

    /** Adds a handler for GET requests to the given path */
    def get ( path: String ): Finalize
        = when( Matcher.method(Method.GET) and Matcher.path(path) )

    /** Adds a handler for POST requests to the given path */
    def post ( path: String ): Finalize
        = when( Matcher.method(Method.POST) and Matcher.path(path) )

    /** Adds a handler for DELETE requests to the given path */
    def delete ( path: String ): Finalize
        = when( Matcher.method(Method.DELETE) and Matcher.path(path) )

    /** Adds a handler for PUT requests to the given path */
    def put ( path: String ): Finalize
        = when( Matcher.method(Method.PUT) and Matcher.path(path) )

    /** Adds a handler for PATCH requests to the given path */
    def patch ( path: String ): Finalize
        = when( Matcher.method(Method.PATCH) and Matcher.path(path) )

    /** Adds a handler for HEAD requests to the given path */
    def head ( path: String ): Finalize
        = when( Matcher.method(Method.HEAD) and Matcher.path(path) )

    /** Adds a handler that matches secure requests */
    lazy val isSecure: Finalize = when( Matcher.isSecure )

    /** Adds a handler that matches non-secure requests */
    lazy val notSecure: Finalize = when( Matcher.notSecure )

    /** Adds a handler when the request has the given subdomain */
    def subdomain( subdomain: String ): Finalize
        = when( Matcher.subdomain(subdomain) )

    /** Adds a handler when the request has a given host name */
    def host( hostname: String ): Finalize = when( Matcher.host(hostname) )

    /** Adds a handler when the request does not have a subdomain */
    lazy val noSubdomain: Finalize = when( Matcher.noSubdomain )

    /** Sets up a handler for the root directory */
    lazy val index: Finalize = request("/")

    /** Uses a custom callback as a matcher */
    def when ( callback: Request => Boolean ): Finalize = {
        when( Matcher.call { request =>
            Matcher.Result( callback(request) )
        } )
    }

    /** Uses a custom thunk as a matcher */
    def when ( callback: => Boolean ): Finalize
        = when( Matcher.call { Matcher.Result( callback ) } )

    /** Inverts the results of another matcher */
    def not( other: Finalize ): Finalize = when( Matcher.not(other.matcher) )

    /** Inverts the results of another matcher */
    def not: Fluent = {
        val outerFluent = this
        val outerCtx = context
        new Fluent {
            override implicit protected val context = outerCtx
            override def when (matcher: Matcher): Finalize
                = outerFluent.when( Matcher.not(matcher) )
        }
    }
}


/**
 * A fluent interface for building Skene dispatchers
 */
class Skene (
    override implicit protected val context: ExecutionContext
) extends Handler with Matcher with Fluent {

    /** The internal dispatcher */
    private val dispatcher = new Dispatcher

    /** {@inheritDoc} */
    override def handle(
        recover: Recover, request: Request, response: Response
    ): Unit = {
        dispatcher.handle( recover, request, response )
    }

    /** {@inheritDoc} */
    override def matches ( request: Request ): Matcher.Result
        = dispatcher.matches( request )

    /** {@inheritDoc} */
    override def when ( matcher: Matcher ): Finalize = new Finalize(
        matcher,
        (matcher: Matcher, handler: Handler) =>
            dispatcher.add(matcher, handler)
    )

    /** Sets up a default handler */
    def default ( handler: Handler ): Unit = dispatcher.default(handler)

    /** Sets up a default handler from a callback */
    def default ( handler: (Request, Response) => Unit ): Unit
        = default( Handler(handler) )

    /** Sets up a default handler from a callback */
    def default ( handler: (Recover, Request, Response) => Unit ): Unit
        = default( Handler(handler) )

    /** Sets up the handler for when exceptions are thrown */
    def error ( handler: Dispatcher.OnError ): Unit
        = dispatcher.error( handler )

    /** Sets up the handler for when exceptions are thrown */
    def error ( handler: Dispatcher.SimpleOnError ): Unit
        = dispatcher.error( handler )

    /** Delegates to another object if it matches */
    def delegate ( to: Handler with Matcher ): Unit = when( to )( to )
}


