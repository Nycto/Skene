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
        ( callback: (Recover, Request, Response) => Unit )
        ( implicit context: ExecutionContext )
    : Handler = new Handler {
        def handle(
            recover: Recover, request: Request, response: Response
        ): Unit
            = recover.from { callback( recover, request, response ) }
    }

    /**
     * Creates a handler from a callback
     */
    def apply
        ( callback: (Request, Response) => Unit )
        ( implicit context: ExecutionContext )
    : Handler = new Handler {
        def handle(
            recover: Recover, request: Request, response: Response
        ): Unit
            = recover.from { callback( request, response ) }
    }

    /**
     * Creates a handler from a callback
     */
    def apply
        ( callback: (Response) => Unit )
        ( implicit context: ExecutionContext )
    : Handler = new Handler {
        def handle(
            recover: Recover, request: Request, response: Response
        ): Unit
            = recover.from { callback( response ) }
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
    def handle( recover: Recover, request: Request, response: Response ): Unit

    /**
     * The primary handler for using a handler has a servlet
     */
    override protected def service (
        request: HttpServletRequest, response: HttpServletResponse
    ): Unit = {

        val async = request.startAsync();

        val wrappedReq = new ServletRequest( request )
        val wrappedResp = new ServletResponse( async, response ).isHtml

        val recover = Recover.errorAndLog( wrappedResp, logger )

        logger.request( wrappedReq )

        recover.async {
            handle( recover, wrappedReq, wrappedResp )
        }
    }

}


