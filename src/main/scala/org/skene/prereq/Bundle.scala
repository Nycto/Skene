package org.skene

import java.lang.reflect.{InvocationHandler, Method, Proxy}

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
 * An invocation handler that collects a map of other objects into one class
 */
class Bundle private[skene] (
    private val joined: Map[Class[_], Any] = Map()
) extends InvocationHandler {

    /** {@inheritDoc} */
    override def toString = {
        val bundled = joined.map( part => {
            "%s: %s".format( part._1.getSimpleName, part._2.toString )
        })

        "Bundle(" + bundled.mkString(", ") + ")"
    }

    /**
     * Returns one of the specific data types from this bundle
     */
    def get[T: Manifest]: T = joined( manifest[T].erasure ).asInstanceOf[T]

    /**
     * Adds a new class to this bundle
     */
    private[skene] def add[T] ( clazz: Class[_], value: T )
        = new Bundle( joined + ((clazz, value)) )

    /** {@inheritDoc} */
    override def invoke(
        proxy: AnyRef, method: Method, args: Array[AnyRef]
    ): AnyRef = {
        method.getName match {
            case "toString" => toString
            case _ => {
                val clazz = method.getDeclaringClass
                joined.get( clazz ) match {
                    case None => throw new Exception(
                        "Class is not in this bundle: " + clazz.getName
                    )
                    case Some(data) => method.invoke( data, args:_* )
                }
            }
        }
    }

}

/**
 * Constructs an instance of the requested type by calling all the
 * registered builders
 */
class PrereqHandler[T] private[skene] (
    private val builders: Registry.Builders,
    private val callback: (T) => Response,
    private val types: Set[Class[_]]
) extends Handler {

    /** @{inheritDoc} */
    override def toString = "PrereqHandler(%s)".format(
        types.map(_.getSimpleName).mkString(", ")
    )

    /** @{inheritDoc} */
    override def handle( req: Request ): Response = {

        // Start piecing together the bundle
        val result = {
            new BundleCollect( builders )
                .add[Prereq]( new Prereq {
                    override val request: Request = req
                } )
                .incorporate( types )
        }

        result match {
            case Left(response) => response
            case Right(collector) => {

                // Bring the bundle together into a proxy object that will
                // mimic the requested interface. Dynamic programming in a
                // static language!
                val proxy = {
                    Proxy.newProxyInstance(
                        this.getClass.getClassLoader,
                        collector.toArray,
                        collector.bundle
                    )
                    .asInstanceOf[T]
                }

                // Use the wrapped callback to process their request
                callback( proxy )
            }
        }
    }

}


