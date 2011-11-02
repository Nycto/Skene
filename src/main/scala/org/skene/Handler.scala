package org.skene

import org.skene.request.ServletRequest

import javax.servlet.http.{HttpServlet,HttpServletRequest,HttpServletResponse}

/**
 * Methods for quickly building small handlers
 */
object Handler {

    def apply( callback: (Request) => Response ): Handler
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
    def handle( request: Request ): Response

    /**
     * The primary handler for using a handler has a servlet
     */
    private def doRequest (
        request: HttpServletRequest, response: HttpServletResponse
    ): Unit = {

        // Set some sensible defaults
        response.setContentType("text/html;charset=utf-8")

        val result = handle( new ServletRequest( request ) )

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
    ( private val callback: (Request) => Response )
    extends Handler
{

    /**
     * An alternate constructor that allows thunks to be used as handlers
     */
    def this ( thunk: => Response ) = this((request: Request) => {
        thunk
    })

    /**
     * @see Handler
     */
    override def handle( request: Request ) = callback(request)

}


