package com.roundeights.skene

import scala.concurrent.{ExecutionContext, Promise, Future}


/**
 * The base class for generated prereq bundles
 */
trait Prereq {

    /**
     * The request that is being processed
     */
    def request: Request

    /**
     * The response that will be sent back
     */
    def response: Response

    /** {@inheritDoc} */
    override def toString = "Prereq(%s)".format( request )

}

/**
 * A Graph is a pre-compiled list of providers in dependency order
 */
class Graph[T] private[skene] (
    private val builders: Registry.Builders,
    private val dependencies: List[Class[_]]
)(
    implicit context: ExecutionContext
) {

    /** {@inheritDoc} */
    override def toString = "Graph(%s)".format(
        dependencies.map(_.getSimpleName).mkString(", ")
    )

    /** Builds a bundle for the given request/response */
    def build ( req: Request, resp: Response ): Future[T] = {
        val promise = Promise[T]()

        def collect ( clazzes: List[Class[_]], bundle: Bundle ): Unit = {
            clazzes match {
                case Nil => promise.success(
                    bundle.asProxyOf[T](classOf[Prereq] :: dependencies)
                )
                case head :: tail => {
                    val future = builders( head ).build( head, bundle )
                    future.onFailure {
                        case err: Throwable => promise.failure( err )
                    }
                    future.onSuccess {
                        case bundle: Bundle => collect( tail, bundle )
                    }
                }
            }
        }

        val initialBundle = new Bundle().add( classOf[Prereq], new Prereq {
            override val request = req
            override val response = resp
        })

        context.execute( new Runnable {
            override def run = collect( dependencies, initialBundle )
        } )

        promise.future
    }

    /** Builds a Request Handler that will be given a fully formed bundle */
    def in ( action: (T, Response) => Unit ): Handler
        = new PrereqHandler[T]( this, action )

    /** Builds a Request Handler that will be given a fully formed bundle */
    def in ( action: (T, Response, Recover) => Unit ): Handler
        = new PrereqHandler[T]( this, action )

    /** An interface for using this Graph in a for comprehension */
    class ForComprehension[I] (
        transform: (T, Response, Recover) => I
    ) {

        /** Generates a handler from a callback */
        def map[O] ( decorator: (I) => O ) = new ForComprehension[O](
            (reqs, resp, rec) => decorator( transform(reqs, resp, rec) )
        )

        /** Generates a handler from a callback */
        def foreach ( action: (I) => Unit ): Handler
            = in( (reqs, resp, rec) => action( transform(reqs, resp, rec) ) )
    }

    /** Returns an interface for using this graph in a for comprehension */
    def withFilter (
        predicate: (Tuple3[T, Response, Recover]) => Boolean
    ) = new ForComprehension[Tuple3[T, Response, Recover]](
        (reqs, resp, rec) => Tuple3(reqs, resp, rec)
    )
}



