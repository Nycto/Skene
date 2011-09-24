package org.skene

/**
 * Methods for quickly building small handlers
 */
object Handler {

    def apply( callback: (Context) => Response ): Handler
        = new CallbackHandler(callback)

    def apply( thunk: => Response ): Handler
        = new CallbackHandler(thunk)
}

/**
 * Defines an object that can handle a request
 */
trait Handler {
    /**
     * Handles the given request and returns the response data
     */
    def handle( context: Context ): Response
}

/**
 * A helper handler that simply converts a callback into a handler
 */
class CallbackHandler
    ( private val callback: (Context) => Response )
    extends Handler
{
    /**
     * An alternate constructor that allows thunks to be used as handlers
     */
    def this ( thunk: => Response ) = this((context: Context) => {
        thunk
    })

    /**
     * @see Handler
     */
    override def handle( context: Context ) = callback(context)
}

