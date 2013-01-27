package com.roundeights.skene

import com.roundeights.skene.request.ServletRequest
import com.roundeights.skene.response.ServletResponse

import javax.servlet.http.{HttpServlet,HttpServletRequest,HttpServletResponse}
import javax.servlet.AsyncContext

import scala.concurrent.ExecutionContext

/**
 * Methods for quickly building small handlers
 */
object Handler {

    /**
     * Creates a handler from a callback
     */
    def apply
        ( callback: (Request, Response) => Unit )
        ( implicit context: ExecutionContext )
    : Handler = new Handler {
        def handle( request: Request, response: Response ): Unit
            = callback( request, response )
    }

    /**
     * Creates a handler from a callback
     */
    def apply
        ( callback: (Response) => Unit )
        ( implicit context: ExecutionContext )
    : Handler = new Handler {
        def handle( request: Request, response: Response ): Unit
            = callback( response )
    }

}

/**
 * Defines an object that can handle a request
 */
abstract class Handler (
    implicit context: ExecutionContext
) extends HttpServlet {

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
        val wrappedResp = new ServletResponse(
            async, response, new Recover( logger )
        ).isHtml

        logger.request( wrappedReq )

        context.execute( new Runnable {
            override def run = wrappedResp.recover {
                // Pass the request off to the handler, but watch for errors
                handle( wrappedReq, wrappedResp )
            }
        } )
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


