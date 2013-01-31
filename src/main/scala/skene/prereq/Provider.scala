package com.roundeights.skene

import scala.concurrent.{ExecutionContext, Promise, Future}


/**
 * An object that has the ability to build a Prerequisite object
 */
trait Provider[T] {

    /**
     * Returns a list of dependencies that must be built before this provider
     * can operate
     */
    def dependencies: Set[Class[_]] = Set()

    /**
     * Builds the data type
     */
    def build( bundle: Bundle, result: Promise[T] ): Unit

    /**
     * An internal method for building this provider
     */
    private[skene] final def build
        ( forClass: Class[_], bundle: Bundle )
        ( implicit context: ExecutionContext )
    : Future[Bundle] = {
        val promise = Promise[T]

        context.execute( new Runnable {
            override def run = try {
                build( bundle, promise )
            } catch {
                case err: Throwable => promise.failure( err )
            }
        } )

        promise.future.map {
            prereq => bundle.add( forClass, prereq )
        }
    }

}

