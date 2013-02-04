package com.roundeights.skene

import java.lang.reflect.{InvocationHandler, Method, Proxy}

/**
 * An invocation handler that collects a map of other objects into one class
 *
 * @param joined A map of classes to their instances
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

    /** Returns one of the specific data types from this bundle */
    def get[T: Manifest]: T
        = joined( manifest[T].runtimeClass ).asInstanceOf[T]

    /** Pulls the skene request object from this bundle */
    def request = get[Prereq].request

    /** Pulls the skene response object from this bundle */
    def response = get[Prereq].response

    /** Adds a new class to this bundle */
    private[skene] def add[T] ( clazz: Class[_], value: T ): Bundle = {
        if ( !clazz.isInstance(value) )
            throw new Exception("Bundle add mismatch")

        new Bundle( joined + ((clazz, value)) )
    }

    /** Adds a new class to this bundle */
    private[skene] def add[T: Manifest] ( value: T ): Bundle
        = add( manifest[T].runtimeClass, value )

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

    /** Returns this bundle as an instance of the given class */
    private[skene] def asProxyOf[U]( clazzes: Seq[Class[_]] ): U = {
        val missing = clazzes.toSet.diff( joined.keySet )

        if ( missing.size > 0 ) {
            throw new Exception(
                "Bundle is missing the following classes: "
                + missing.map( _.getSimpleName ).mkString(", ")
            )
        }

        Proxy.newProxyInstance(
            this.getClass.getClassLoader,
            clazzes.toArray,
            this
        )
        .asInstanceOf[U]
    }

}

