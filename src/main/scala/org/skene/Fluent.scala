package org.skene

import org.skene._


/**
 * A helper class for fluently building a dispatcher
 */
class Fluent (
    private val dispatcher: Dispatcher,
    private val matcher: Matcher
) {

    def apply ( callback: (Request) => Response ): Unit
        = dispatcher.add( matcher, Handler(callback) )

    def apply ( callback: => Response ): Unit
        = dispatcher.add( matcher, Handler(callback) )

    def apply ( handler: Handler ): Unit
        = dispatcher.add( matcher, handler )
}


/**
 * A fluent interface for building Skene dispatchers
 */
trait Skene extends Handler {

    /**
     * The dispatcher to collect into
     */
    private val dispatcher = new Dispatcher

    /**
     * @see Handler
     */
    override def handle( request: Request ): Response
        = dispatcher.handle( request )

    /**
     * Adds a handler for any request matching the given path
     */
    def request ( path: String ): Fluent
        = new Fluent( dispatcher, Matcher.path(path) )

    /**
     * Sets up a default handler
     */
    val default = new Fluent( dispatcher, Matcher.always )

    /**
     * Sets up a handler for the root directory
     */
    val index = new Fluent( dispatcher, Matcher.path("/") )
}


/**
 * The base class for Skene servers
 */
abstract class SkeneApp ( private val port: Int = 80 ) extends Skene {
    def main(args : Array[String]): Unit = Server( port, this )
}

