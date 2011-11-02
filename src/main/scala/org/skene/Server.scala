package org.skene

import org.eclipse.jetty.server.{Server => JettyServer}
import org.eclipse.jetty.servlet.ServletContextHandler;
import org.eclipse.jetty.servlet.ServletHolder;

import org.skene.request._

/**
 * Helper methods for starting a server
 */
object Server {

    def apply ( port: Int, handler: Handler ): Unit = {

        val server = new JettyServer(port)
        server.setSendServerVersion(false)

        val contextHandler = new ServletContextHandler( server, "/" )
        contextHandler.addServlet(new ServletHolder(handler), "/")

        server.start();
        server.join();
    }

    def apply ( port: Int, handler: => Response ): Unit
        = apply( port, Handler(handler) )

    def apply ( port: Int, handler: (Request) => Response ): Unit
        = apply( port, Handler(handler) )

}

