package test.scala.com.skene.response

import org.specs2.mutable._
import org.specs2.mock._

import com.roundeights.skene.response.ServletResponse

import java.io.{ByteArrayOutputStream, ByteArrayInputStream}
import java.util.zip.GZIPInputStream
import javax.servlet.http.{HttpServletResponse, HttpServletRequest}
import javax.servlet.{AsyncContext, ServletOutputStream}
import com.roundeights.skene.Logger
import scala.concurrent._
import scala.concurrent.duration._

class ServletTest extends Specification with Mockito {

    // A mock servlet stream that just stores whatever is written to it
    class StreamStub extends ServletOutputStream {
        private val out = new ByteArrayOutputStream
        override def write( byte: Int ): Unit = out.write(byte)
        override def toString = out.toString( "UTF8" )
        def ungzipped: String = {
            val stream = new GZIPInputStream(
                new ByteArrayInputStream( out.toByteArray ) )
            scala.io.Source.fromInputStream(stream).getLines().mkString("\n")
        }
    }

    // Returns a mock response and a blocking method call that will wait
    // until the request is finalized
    def mocks = {
        val promise = Promise[Unit]
        val async = mock[AsyncContext]
        async.complete() answers { _ => promise.success( Unit ) }

        val request = mock[HttpServletRequest]
        val response = mock[HttpServletResponse]
        val stream = new StreamStub

        response.getOutputStream returns stream

        val servlet = new ServletResponse(
            async, request, response, mock[Logger], 0)

        val wait = () => Await.result(promise.future, Duration(3, "second"))

        (servlet, request, response, stream, wait)
    }

    "A Servlet Response" should {

        "Return raw content when a client doesnt send an Accept header" in {
            val (servlet, request, response, stream, wait) = mocks
            servlet.content("testing")
            servlet.done
            wait()

            stream.toString must_== "testing"
        }

        "Send back raw content when gzip isn't support" in {
            val (servlet, request, response, stream, wait) = mocks
            request.getHeader("Accept-Encoding") returns "something else"

            servlet.content("testing")
            servlet.done
            wait()

            stream.toString must_== "testing"
        }

        "Send back gzipped content when gzip is support" in {
            val (servlet, request, response, stream, wait) = mocks
            request.getHeader("Accept-Encoding") returns "gzip"

            servlet.content("testing")
            servlet.done
            wait()

            stream.ungzipped must_== "testing"
        }

        "Not write back any content for HEAD requests" in {
            val (servlet, request, response, stream, wait) = mocks
            request.getMethod returns "HEAD"

            servlet.content("testing")
            servlet.done
            wait()

            stream.toString must_== ""
        }
    }
}

