package org.skene

import org.skene._


/**
 * A fluent interface for building Skene dispatchers
 */
trait Skene extends Handler {

    /**
     * The dispatcher to collect into
     */
    private val dispatcher = new Dispatcher( logger )

    /**
     * A helper class for fluently building a dispatcher
     */
    class Fluent ( private val matcher: Matcher ) {

        def apply ( callback: (Request) => Response ): Unit
            = dispatcher.add( matcher, Handler(callback) )

        def apply ( callback: => Response ): Unit
            = dispatcher.add( matcher, Handler(callback) )

        def apply ( handler: Handler ): Unit
            = dispatcher.add( matcher, handler )

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

    /**
     * @see Handler
     */
    override def handle( request: Request ): Response
        = dispatcher.handle( request )

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
    def default ( handler: Handler ): Unit = dispatcher.default( handler )

    /**
     * Sets up a default handler from the thunk
     */
    def default ( handler: => Response ): Unit = default( Handler(handler) )

    /**
     * Sets up a default handler from a callback
     */
    def default ( handler: (Request) => Response ): Unit
        = default( Handler(handler) )

    /**
     * Sets up the handler for when exceptions are thrown
     */
    def error ( handler: (Throwable, Request) => Response ): Unit
        = dispatcher.error( handler )

    /**
     * Sets up a handler for the root directory
     */
    lazy val index: Fluent = request("/")

    /**
     * Uses a custom callback as a matcher
     */
    def when ( callback: Request => Boolean ) = {
        new Fluent( Matcher.call { request =>
            Matcher.Result( callback(request) )
        } )
    }

    /**
     * Uses a custom thunk as a matcher
     */
    def when ( callback: => Boolean )
        = new Fluent( Matcher.call { Matcher.Result( callback ) } )

}


/**
 * The base class for Skene servers
 */
abstract class SkeneApp ( private val port: Int = 80 ) extends Skene {
    def main(args : Array[String]): Unit = Server( port, this )
}

