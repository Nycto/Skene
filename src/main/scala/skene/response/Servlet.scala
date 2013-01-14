package com.roundeights.skene.response

import scala.actors.Actor
import scala.collection.mutable.MutableList

import javax.servlet.http.HttpServletResponse
import javax.servlet.AsyncContext

import com.roundeights.skene.Response
import com.roundeights.skene.Renderable

/**
 * A response that wraps a servlet
 */
class ServletResponse (
    async: AsyncContext,
    response: HttpServletResponse
) extends ActorResponse {

    /**
     * An actor this is used to communicate with the Servlet Response
     */
    protected val actor: Actor = Actor.actor {

        val data = MutableList[Renderable]()
        val writer = response.getWriter

        def flush (): Unit = {
            data.map( _.render( writer ) )
            data.clear()
            writer.flush()
        }

        Actor.loop {
            Actor.react {
                case Response.Header( field, value ) =>
                    response.setHeader( field.name, value )

                case code: Response.Code => response.setStatus( code.code )

                case content: Renderable => data += content

                case _: Response.Flush => flush()

                case _: Response.Done => {
                    flush()
                    writer.close()
                    async.complete()
                    Actor.exit()
                }
            }
        }
    }

}


