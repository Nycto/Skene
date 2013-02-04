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

    /** {@inheritDoc} */
    override def handle(
        recover: Recover, req: Request, resp: Response
    ): Unit = {
        val future: Future[T] = graph.build( req, resp )

        recover.fromFuture( future )

        future.onSuccess {
            case bundle => recover.from {
                action( bundle, resp )
            }
        }
    }

}


