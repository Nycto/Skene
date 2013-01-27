package com.roundeights.skene

import scala.concurrent.ExecutionContext
import scala.reflect.ClassTag

/**
 * Wraps a list of classes to provide easy access to their properties
 */
private class ClassList ( val clazzes: Set[Class[_]] ) {

    /**
     * Asserts that none of the classes in this class list have conflicting
     * method interfaces
     */
    clazzes.foldLeft( Set[String]() ) ( (methods, clazz) => {

        // Get all the methods in this class, except for toString
        val newMethods = clazz.getMethods.map( _.getName ).toSet - "toString"

        val overlap = methods.intersect( newMethods )
        if ( overlap.size > 0 )
            throw new Registry.ConflictingPrereqs( overlap )

        methods ++ newMethods
    } )

    /**
     * Creates a new class list from a set of manifests
     */
    def this ( clazzes: ClassTag[_]* )
        = this( Set( clazzes:_* ).map(_.runtimeClass) )

}

/**
 * The internal implementation of the Registry. This is separated out
 * to simplify the Registry class
 */
private class RegistryData
    ( val builders: Map[Class[_], Provider[_]] = Map() )
    ( implicit context: ExecutionContext )
{

    /**
     * A set of all the registered prereqs
     */
    private val registered: Set[Class[_]] = builders.keySet

    /**
     * Registers a new builder for a given type
     */
    def register[T: Manifest] ( builder: Provider[T] ): RegistryData
        = new RegistryData( builders + ((manifest[T].runtimeClass, builder)) )

    /**
     * Registers a callback to act as a Prereq provider
     */
    def register[T: Manifest] (
        builder: (Bundle, Continue[T]) => Unit,
        depends: Class[_]*
    ): RegistryData = {
        register( new Provider[T] {
            override def build( bundle: Bundle, next: Continue[T] ): Unit
                = builder(bundle, next)
            override def dependencies = depends.toSet
        } )
    }

    /**
     * Registers a callback to act as a Prereq provider
     */
    def register[T: Manifest] (
        builder: ( Continue[T] ) => Unit,
        depends: Class[_]*
    ): RegistryData = {
        register( new Provider[T] {
            override def build( bundle: Bundle, next: Continue[T] ): Unit
                = builder( next )
            override def dependencies = depends.toSet
        } )
    }

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
     * Constructs an instance of the requested type by calling all the
     * registered builders
     */
    def build[T](
        callback: (T, Response) => Unit, clazzList: ClassList
    ): Handler = new PrereqHandler[T](
        builders,
        callback,
        dependenciesOf( clazzList.clazzes )
    )

}

