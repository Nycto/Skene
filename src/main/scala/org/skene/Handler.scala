package org.skene

import org.skene.context.JettyContext

import javax.servlet.http.{HttpServlet,HttpServletRequest,HttpServletResponse}

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
trait Handler extends HttpServlet {

    /**
     * Handles the given request and returns the response data
     */
    def handle( context: Context ): Response

    /**
     * The primary handler for using a handler has a servlet
     */
    private def doRequest (
        request: HttpServletRequest, response: HttpServletResponse
    ): Unit = {

        // Set some sensible defaults
        response.setContentType("text/html;charset=utf-8")

        val result = handle( new JettyContext( request, response ) )

        // Change the status code
        response.setStatus( result.code.code )

        // Add in any custom headers
        result.headers.foreach { (header) =>
            response.setHeader( header._1, header._2 )
        };

        result.content.render( response.getWriter )
    }

    /**
     * @see HttpServlet
     */
    override def doGet (
        request: HttpServletRequest, response: HttpServletResponse
    ): Unit = doRequest( request, response )

    /**
     * @see HttpServlet
     */
    override def doPost (
        request: HttpServletRequest, response: HttpServletResponse
    ): Unit = doRequest( request, response )

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


