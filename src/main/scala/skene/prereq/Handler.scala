package com.roundeights.skene

import scala.concurrent.{ExecutionContext, Future}

/**
 * Constructs an instance of the requested type by calling all the
 * registered builders
 */
class PrereqHandler[T] private[skene] (
    private val graph: Graph[T],
    private val action: (T, Response) => Unit
) (
    implicit context: ExecutionContext
) extends Handler {

    /** {@inheritDoc} */
    override def toString = "PrereqHandler(%s)".format( graph )

    /** Handles any errors thrown by the prereqs */
    private def onError ( resp: Response, err: Throwable ): Unit = {
        resp.serverError.html(
            <html>
                <head><title>Internal Server Error</title></head>
                <body><h1>Internal Server Error</h1></body>
            </html>
        ).done
    }

    /** {@inheritDoc} */
    override def handle( req: Request, resp: Response ): Unit = {
        val future: Future[T] = graph.build( req, resp )
        future.onFailure { case err: Throwable => onError(resp, err) }

        future.onSuccess {
            case bundle => try {
                action( bundle, resp );
            } catch {
                case err: Throwable => onError(resp, err)
            }
        }
    }

}


