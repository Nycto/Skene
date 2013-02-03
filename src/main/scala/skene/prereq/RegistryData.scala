package com.roundeights.skene

import scala.concurrent.{ExecutionContext, Promise}
import scala.reflect.ClassTag


/**
 * The internal implementation of the Registry. This is separated out
 * to simplify the Registry class
 */
private class RegistryData
    ( val builders: Map[Class[_], Provider[_]] = Map() )
    ( implicit context: ExecutionContext )
{

    /**
     * Registers a new builder for a given type
     */
    def register[T: Manifest] ( builder: Provider[T] ): RegistryData
        = new RegistryData( builders + ((manifest[T].runtimeClass, builder)) )

    /**
     * Registers a callback to act as a Prereq provider
     */
    def register[T: Manifest] (
        builder: (Bundle, Promise[T]) => _,
        depends: Class[_]*
    ): RegistryData = register(
        new Provider[T] {
            override def build( bundle: Bundle, result: Promise[T] ): Unit
                = builder(bundle, result)
            override def dependencies = depends.toSet
        }
    )

    /**
     * Registers a callback to act as a Prereq provider
     */
    def register[T: Manifest] (
        builder: ( Promise[T] ) => _,
        depends: Class[_]*
    ): RegistryData = register(
        new Provider[T] {
            override def build( bundle: Bundle, result: Promise[T] ): Unit
                = builder( result )
            override def dependencies = depends.toSet
        }
    )

    /**
     * Returns a linearized list of all the dependencies of a set of classes
     */
    def dependenciesOf ( clazzes: Set[Class[_]] ): List[Class[_]] = {

        def collect (
            from: Set[Class[_]], onto: List[Class[_]]
        ): List[Class[_]] = {
            from.foldLeft[ List[Class[_]] ]( onto ) {
                (accum, clazz) => {
                    if ( !builders.contains(clazz) )
                        throw new Registry.UnregisteredPrereq( clazz )
                    else
                        collect( builders(clazz).dependencies, clazz :: accum )
                }
            }
        }

        collect( clazzes, Nil ).distinct
    }

    /**
     * Asserts that none of a given set of classes have conflicting interfaces
     */
    def checkConflicts ( clazzes: Seq[Class[_]] ): Unit = {
        clazzes.foldLeft( Set[String]() ) ( (methods, clazz) => {

            // Get all the methods in this class, except for toString
            val newMethods = clazz.getMethods.map(_.getName).toSet - "toString"

            val overlap = methods.intersect( newMethods )
            if ( overlap.size > 0 )
                throw new Registry.ConflictingPrereqs( overlap )

            methods ++ newMethods
        } )
    }

    /**
     * Constructs an instance of the requested type by calling all the
     * registered builders
     */
    def build[T]( clazzes: Set[Class[_]] ): Graph[T] = {
        val resolved = dependenciesOf( clazzes )
        checkConflicts( resolved )
        new Graph[T]( builders, resolved )
    }

    /**
     * Constructs an instance of the requested type by calling all the
     * registered builders
     */
    def build[T]( clazzes: ClassTag[_]* ): Graph[T]
        = build( Set( clazzes:_* ).map(_.runtimeClass) )

}


