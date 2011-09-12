package main.scala.com.example

import javax.servlet.http.{HttpServletRequest => ServletRequest}
import javax.servlet.http.{HttpServletResponse => ServletResponse}
import javax.servlet.ServletException

import org.eclipse.jetty.server.handler.AbstractHandler
import org.eclipse.jetty.server.{Server => ServletServer}
import org.eclipse.jetty.server.{Request => RawJettyRequest}

/**
 * Example Jetty application
 */
object Start {
    def main(args : Array[String]): Unit = {
        val server = new ServletServer(8080)
        server.setHandler( new ExampleHandler );
        server.start();
        server.join();
    }
}

/**
 * A Jetty requestion handler
 */
private class ExampleHandler extends AbstractHandler {

    override def handle(
        target: String,
        baseRequest: RawJettyRequest,
        request: ServletRequest,
        response: ServletResponse
    ): Unit = {

        response.setContentType("text/html; charset=UTF-8")
        response.setStatus(ServletResponse.SC_OK)

        response.getWriter.write("<h1>Example</h1>");

        baseRequest.setHandled(true)
    }

}

