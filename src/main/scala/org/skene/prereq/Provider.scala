package org.skene


/**
 * The mechanism that allows one provider to trigger the next while
 * building a Bundle
 */
class Continue[T] private[skene] (
    private val forClazz: Class[_],
    private val bundle: Bundle,
    private val next: (Bundle) => Unit
) {

    /**
     * Continues to the next provider
     */
    def apply( data: T ): Unit = next( bundle.add(forClazz, data) )

}

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
    def build( bundle: Bundle, next: Continue[T] ): Unit

    /**
     * An internal method for building this provider
     */
    private[skene] final def build(
        forClazz: Class[_], bundle: Bundle, next: (Bundle) => Unit
    ): Unit = build( bundle, new Continue[T](forClazz, bundle, next) )

}

