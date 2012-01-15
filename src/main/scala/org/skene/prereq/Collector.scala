package org.skene


/**
 * A helper class for collecting info about the classes being bundled
 */
private class BundleCollect (
    val builders: Registry.Builders,
    val bundle: Bundle = new Bundle,
    val clazzes: Set[Class[_]] = Set()
) {

    /**
     * Adds a new class to this collection
     */
    private def add[T] ( clazz: Class[_], data: T ): BundleCollect = {
        new BundleCollect(
            builders,
            bundle.add( clazz, data ),
            clazzes + clazz
        )
    }

    /**
     * Adds a new object to this collection
     */
    def add[T: Manifest]( data: T ): BundleCollect
        = add( manifest[T].erasure, data )

    /**
     * Adds a new class and all of its dependencies
     */
    def incorporate( clazz: Class[_] ): Either[Response, BundleCollect] = {

        // Grab the builder for this class. At this point, the Registry should
        // have already guaranteed the existence of this element, so no need
        // to do any additional checks
        val builder = builders(clazz)

        val dependsOn = builder.dependencies.diff( clazzes )

        // Add in all the dependencies, and their dependencies
        val resolved = dependsOn.foldLeft[Either[Response,BundleCollect]] (
            Right(this)
        ) {
            (accum: Either[Response,BundleCollect], dependency: Class[_]) => {
                accum match {
                    case response: Left[_, _] => response
                    case Right(collected) => collected.incorporate(dependency)
                }
            }
        }

        // Finish by adding in the requested class
        resolved match {
            case response: Left[_, _] => response
            case Right(collected) => builder.build( collected.bundle ) match {
                case Left(response) => Left(response)
                case Right(prereq) => Right( collected.add( clazz, prereq ) )
            }
        }

    }

    /**
     * Incorporates a list of classes
     */
    def incorporate (
        clazzes: Set[Class[_]]
    ): Either[Response, BundleCollect] = {
        clazzes.foldLeft[Either[Response,BundleCollect]] ( Right(this) ) {
            (accum: Either[Response,BundleCollect], clazz: Class[_]) => {
                accum match {
                    case response: Left[_, _] => response
                    case Right(collected) => collected.incorporate(clazz)
                }
            }
        }
    }

    /**
     * Returns an array of all the classes this collection contains
     */
    def toArray = clazzes.toArray

}

