package org.skene

import org.skene.request.ServletRequest
import org.skene.response.ServletResponse

import scala.actors.Actor

import javax.servlet.http.{HttpServlet,HttpServletRequest,HttpServletResponse}
import javax.servlet.AsyncContext

/**
 * Methods for quickly building small handlers
 */
object Handler {

    /**
     * Creates a handler from a callback
     */
    def apply( callback: (Request, Response) => Unit ): Handler = new Handler {
        def handle( request: Request, response: Response ): Unit
            = callback( request, response )
    }

    /**
     * Creates a handler from a callback
     */
    def apply( callback: (Response) => Unit ): Handler = new Handler {
        def handle( request: Request, response: Response ): Unit
            = callback( response )
    }

}

/**
 * Defines an object that can handle a request
 */
trait Handler extends HttpServlet {

    /**
     * The default logger to be overridden.
     */
    protected val logger: Logger = Logger.logger

    /**
     * Handles the given request and returns the response data
     */
    def handle( request: Request, response: Response ): Unit

    /**
     * The primary handler for using a handler has a servlet
     */
    private def doRequest (
        request: HttpServletRequest, response: HttpServletResponse
    ): Unit = {

        val async = request.startAsync();

        val wrappedReq = new ServletRequest( request )
        val wrappedResp = new ServletResponse( async, response ).isHtml

        logger.request( wrappedReq )

        Actor.actor {

            try {
                // Pass the request off to the handler, but watch for errors
                handle( wrappedReq, wrappedResp )
            }
            catch { case err: Throwable => {
                logger.error( err )
                wrappedResp.serverError.html(
                    <html>
                        <head><title>500 Internal Server Error</title></head>
                        <body><h1>500 Internal Server Error</h1></body>
                    </html>
                ).done()
            } }

        }
    }

    /** {@inheritDoc} */
    override def doGet (
        request: HttpServletRequest, response: HttpServletResponse
    ): Unit = doRequest( request, response )

    /** {@inheritDoc} */
    override def doPost (
        request: HttpServletRequest, response: HttpServletResponse
    ): Unit = doRequest( request, response )

    /** {@inheritDoc} */
    override def doPut (
        request: HttpServletRequest, response: HttpServletResponse
    ): Unit = doRequest( request, response )

    /** {@inheritDoc} */
    override def doDelete (
        request: HttpServletRequest, response: HttpServletResponse
    ): Unit = doRequest( request, response )

}


