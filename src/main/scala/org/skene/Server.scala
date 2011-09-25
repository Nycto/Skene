package org.skene

import javax.servlet.http.{HttpServletRequest => ServletRequest}
import javax.servlet.http.{HttpServletResponse => ServletResponse}
import javax.servlet.ServletException

import org.eclipse.jetty.server.handler.AbstractHandler
import org.eclipse.jetty.server.handler.ContextHandler
import org.eclipse.jetty.server.{Server => JettyServer}
import org.eclipse.jetty.server.{Request => RawJettyRequest}

import org.skene.context._

/**
 * Helper methods for starting a server
 */
object Server {

    def apply ( port: Int, handler: Handler ): Unit = {
        val server = new JettyServer(port)
        server.setHandler( new JettyAdapter(handler) )
        server.setSendServerVersion(false)
        server.start();
        server.join();
    }

    def apply ( port: Int, handler: => Response ): Unit
        = apply( port, Handler(handler) )

    def apply ( port: Int, handler: (Context) => Response ): Unit
        = apply( port, Handler(handler) )
}

/**
 * Handles the hand-off between Servlet requests and the
 * custom request handler interface
 */
private class JettyAdapter ( private val handler: Handler )
    extends AbstractHandler {

    override def handle(
        target: String,
        baseRequest: RawJettyRequest,
        request: ServletRequest,
        response: ServletResponse
    ): Unit = {

        // Set some sensible defaults
        response.setContentType("text/html;charset=utf-8")

        val result = handler.handle( new JettyContext( request, response ) )

        // Change the status code
        response.setStatus( result.code.code )

        // Add in any custom headers
        result.headers.foreach { (header) =>
            response.setHeader( header._1, header._2 )
        };

        result.content.render( response.getWriter )

        baseRequest.setHandled(true)
    }
}

