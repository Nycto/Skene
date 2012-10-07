package org.skene

/**
 * The base class for generated prereq bundles
 */
trait Prereq {

    /**
     * The request that is being processed
     */
    def request: Request

    /** {@inheritDoc} */
    override def toString = "Prereq(%s)".format( request )

}

/**
 * Constructs an instance of the requested type by calling all the
 * registered builders
 *
 * @param builders The map of all the registered providers
 * @param callback The callback to invoke once all the providers have passed
 * @param depend All the required data types
 */
class PrereqHandler[T] private[skene] (
    builders: Registry.Builders,
    private val callback: (T, Response) => Unit,
    private val depend: List[Class[_]],
    private val threader: ( => Unit ) => Unit
) extends Handler {

    /** @{inheritDoc} */
    override def toString = "PrereqHandler(%s)".format(
        depend.map(_.getSimpleName).mkString(", ")
    )

    /** @{inheritDoc} */
    override def handle( req: Request, resp: Response ): Unit = {

        // The method that gets invoked once all the providers have
        // successfully executed
        def finalStep ( bundle: Bundle ): Unit
            = callback( bundle.asProxyOf[T](depend), resp )

        // One of the midway functions that actually executes a Provider
        def middleStep
            ( forClazz: Class[_], next: (Bundle) => Unit )
            ( bundle: Bundle ): Unit
        = threader {
            builders(forClazz).build( forClazz, bundle, next )
        }

        depend.foldRight[(Bundle) => Unit](
            finalStep(_)
        )(
            middleStep(_, _)
        )(
            new Bundle().add( classOf[Prereq], new Prereq {
                override val request = req
            })
        )
    }

}


