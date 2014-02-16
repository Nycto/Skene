package com.roundeights.skene.response

import scala.io.Codec
import scala.actors.Actor
import scala.collection.mutable.MutableList
import java.util.concurrent.atomic.AtomicReference
import java.util.zip.GZIPOutputStream

import javax.servlet.http.{HttpServletResponse, HttpServletRequest}
import javax.servlet.AsyncContext

import com.roundeights.skene.{Response, Renderable, Recover, Cookie, Logger}

/**
 * A response that wraps a servlet
 */
class ServletResponse (
    async: AsyncContext,
    request: HttpServletRequest,
    response: HttpServletResponse,
    logger: Logger,
    requestID: Long
) extends ActorResponse {

    /** The status code being sent back */
    private val responseCode
        = new AtomicReference[Response.Code]( Response.Code.OK() )

    /** Whether this request supports gzip encoding */
    private lazy val gzip = request.getHeader("Accept-Encoding") match {
        case null => false
        case accept => accept.toLowerCase.contains("gzip")
    }

    /** The output stream for this response */
    private lazy val stream = gzip match {
        case true => {
            response.setHeader("Content-Encoding", "gzip")
            new GZIPOutputStream( response.getOutputStream )
        }
        case false => response.getOutputStream
    }

    /** An actor this is used to communicate with the Servlet Response */
    protected val actor: Actor = Actor.actor {

        val data = MutableList[Renderable]()

        def flush(): Unit = {
            if ( data.length > 0 ) {
                data.map( _.render( stream, Codec.UTF8 ) )
                data.clear()
                stream.flush()
            }
        }

        Actor.loop {
            Actor.react {
                case Response.Header( field, value ) =>
                    response.setHeader( field.name, value )

                case code: Response.Code => {
                    response.setStatus( code.code )
                    responseCode.set( code )
                }

                case content: Renderable if request.getMethod != "HEAD"
                    => data += content

                case cookie: Cookie => response.addCookie(cookie.toJavaCookie)

                case _: Response.Flush => flush()

                case _: Response.Done => {
                    flush()
                    stream.close()
                    async.complete()
                    logger.response( requestID, responseCode.get )
                    Actor.exit()
                }
            }
        }
    }

}


