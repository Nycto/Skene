package org.skene


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
    def build( bundle: Bundle ): Either[Response,T]

}

/**
 * The internal implementation of the Registry. This is separated out
 * to simplify the Registry class
 */
private class RegistryData( val builders: Registry.Builders = Map() ) {

    /**
     * A set of all the registered prereqs
     */
    private val registered = builders.keySet

    /**
     * Registers a new builder for a given type
     */
    def register[T: Manifest] ( builder: Provider[T] ): RegistryData
        = new RegistryData( builders + ((manifest[T].erasure, builder)) )

    /**
     * Registers a callback to act as a Prereq provider
     */
    def register[T: Manifest] (
        builder: (Bundle) => Either[Response,T],
        depends: Class[_]*
    ): RegistryData = {
        register( new Provider[T] {
            override def build( bundle: Bundle ) = builder(bundle)
            override def dependencies = depends.toSet
        } )
    }

    /**
     * Registers a callback to act as a Prereq provider
     */
    def register[T: Manifest] (
        builder: () => Either[Response,T],
        depends: Class[_]*
    ): RegistryData = {
        register( new Provider[T] {
            override def build( bundle: Bundle ) = builder()
            override def dependencies = depends.toSet
        } )
    }

    /**
     * Constructs an instance of the requested type by calling all the
     * registered builders
     */
    def build[T](
        callback: (T) => Response, clazzes: Set[ClassManifest[_]]
    ): Handler = {
        val clazzSet = clazzes.map(_.erasure)

        // Make sure all of the requested prereqs have been registered
        val missing = clazzSet.diff( registered )
        if ( missing.size > 0 )
            throw new Registry.UnregisteredPrereq( missing )

        // Make sure none of the prereq interfaces have conflicting methods
        clazzSet.foldLeft( Set[String]() ) ( (methods, clazz) => {
            val newMethods = clazz.getMethods.map( _.getName ).toSet

            val overlap = methods.intersect( newMethods )
            if ( overlap.size > 0 )
                throw new Registry.ConflictingPrereqs( overlap )

            methods ++ newMethods
        } )

        new PrereqHandler[T](builders, callback, clazzSet)
    }

}

/**
 * Companion for a Registry
 */
object Registry {

    /**
     * Thrown when attempting to use a Prereq without a registered provider
     */
    class UnregisteredPrereq ( missing: Set[Class[_]] ) extends Exception(
        "Attempting to use an unregistered Prereq: %s".format(
            missing.map( _.getSimpleName ).mkString(", ")
        )
    )

    /**
     * Thrown when two prereqs have conflicting interfaces
     */
    class ConflictingPrereqs ( conflicts: Set[String] ) extends Exception(
        "Incompatible Prereq data types. The following methods are"
        + " defined in multiple classes: " + conflicts.mkString(", ")
    )

    /**
     * The type for the internal builders lookup table
     */
    private[skene] type Builders = Map[Class[_], Provider[_]]

    /**
     * Builds a new Registry
     */
    def apply(): Registry = new Registry

}

/**
 * A registry for collecting available Prerequisite Providers and combining
 * them into a Bundle
 */
class Registry private ( private val inner: RegistryData ) {

    /**
     * The public constructor
     */
    def this() = this( new RegistryData )

    /**
     * Registers a new builder for a given type
     */
    def register[T: Manifest] ( builder: Provider[T] ): Registry
        = new Registry( inner.register(builder) )

    /**
     * Registers a callback to act as a Prereq provider
     */
    def register[T: Manifest] (
        builder: (Bundle) => Either[Response,T],
        depends: Class[_]*
    ): Registry
        = new Registry( inner.register(builder, depends:_* ) )

    /**
     * Registers a callback to act as a Prereq provider
     */
    def register[T: Manifest] (
        builder: () => Either[Response,T],
        depends: Class[_]*
    ): Registry
        = new Registry( inner.register(builder, depends:_* ) )

    /**
     * Builds a new bundle of the given type
     */
    def apply[
        A: Manifest
    ] ( callback: (
        Prereq with A
    ) => Response ): Handler
        = inner.build( callback, Set(
            manifest[A]
        ) )

    /**
     * Builds a new bundle of the given type
     */
    def apply[
        A: Manifest, B: Manifest
    ] ( callback: (
        Prereq with A with B
    ) => Response ): Handler
        = inner.build( callback, Set(
            manifest[A], manifest[B]
        ) )

    /**
     * Builds a new bundle of the given type
     */
    def apply[
        A: Manifest, B: Manifest, C: Manifest
    ] ( callback: (
        Prereq with A with B with C
    ) => Response ): Handler
        = inner.build( callback, Set(
            manifest[A], manifest[B], manifest[C]
        ) )

    /**
     * Builds a new bundle of the given type
     */
    def apply[
        A: Manifest, B: Manifest, C: Manifest, D: Manifest
    ] ( callback: (
        Prereq with A with B with C with D
    ) => Response ): Handler
        = inner.build( callback, Set(
            manifest[A], manifest[B], manifest[C], manifest[D]
        ) )

    /**
     * Builds a new bundle of the given type
     */
    def apply[
        A: Manifest, B: Manifest, C: Manifest, D: Manifest, E: Manifest
    ] ( callback: (
        Prereq with A with B with C with D with E
    ) => Response ): Handler
        = inner.build( callback, Set(
            manifest[A], manifest[B], manifest[C], manifest[D], manifest[E]
        ) )

    /**
     * Builds a new bundle of the given type
     */
    def apply[
        A: Manifest, B: Manifest, C: Manifest, D: Manifest, E: Manifest,
        F: Manifest
    ] ( callback: (
        Prereq with A with B with C with D with E with F
    ) => Response ): Handler
        = inner.build( callback, Set(
            manifest[A], manifest[B], manifest[C], manifest[D], manifest[E],
            manifest[F]
        ) )

    /**
     * Builds a new bundle of the given type
     */
    def apply[
        A: Manifest, B: Manifest, C: Manifest, D: Manifest, E: Manifest,
        F: Manifest, G: Manifest
    ] ( callback: (
        Prereq with A with B with C with D with E with F with G
    ) => Response ): Handler
        = inner.build( callback, Set(
            manifest[A], manifest[B], manifest[C], manifest[D], manifest[E],
            manifest[F], manifest[G]
        ) )

    /**
     * Builds a new bundle of the given type
     */
    def apply[
        A: Manifest, B: Manifest, C: Manifest, D: Manifest, E: Manifest,
        F: Manifest, G: Manifest, H: Manifest
    ] ( callback: (
        Prereq with A with B with C with D with E with F with G with H
    ) => Response ): Handler
        = inner.build( callback, Set(
            manifest[A], manifest[B], manifest[C], manifest[D], manifest[E],
            manifest[F], manifest[G], manifest[H]
        ) )

    /**
     * Builds a new bundle of the given type
     */
    def apply[
        A: Manifest, B: Manifest, C: Manifest, D: Manifest, E: Manifest,
        F: Manifest, G: Manifest, H: Manifest, I: Manifest
    ] ( callback: (
        Prereq with A with B with C with D with E with F with G with H with I
    ) => Response ): Handler
        = inner.build( callback, Set(
            manifest[A], manifest[B], manifest[C], manifest[D], manifest[E],
            manifest[F], manifest[G], manifest[H], manifest[I]
        ) )

    /**
     * Builds a new bundle of the given type
     */
    def apply[
        A: Manifest, B: Manifest, C: Manifest, D: Manifest, E: Manifest,
        F: Manifest, G: Manifest, H: Manifest, I: Manifest, J: Manifest
    ] ( callback: (
        Prereq with A with B with C with D with E with F with G with H with I
        with J
    ) => Response ): Handler
        = inner.build( callback, Set(
            manifest[A], manifest[B], manifest[C], manifest[D], manifest[E],
            manifest[F], manifest[G], manifest[H], manifest[I], manifest[J]
        ) )

    /**
     * Builds a new bundle of the given type
     */
    def apply[
        A: Manifest, B: Manifest, C: Manifest, D: Manifest, E: Manifest,
        F: Manifest, G: Manifest, H: Manifest, I: Manifest, J: Manifest,
        K: Manifest
    ] ( callback: (
        Prereq with A with B with C with D with E with F with G with H with I
        with J with K
    ) => Response ): Handler
        = inner.build( callback, Set(
            manifest[A], manifest[B], manifest[C], manifest[D], manifest[E],
            manifest[F], manifest[G], manifest[H], manifest[I], manifest[J],
            manifest[K]
        ) )

    /**
     * Builds a new bundle of the given type
     */
    def apply[
        A: Manifest, B: Manifest, C: Manifest, D: Manifest, E: Manifest,
        F: Manifest, G: Manifest, H: Manifest, I: Manifest, J: Manifest,
        K: Manifest, L: Manifest
    ] ( callback: (
        Prereq with A with B with C with D with E with F with G with H with I
        with J with K with L
    ) => Response ): Handler
        = inner.build( callback, Set(
            manifest[A], manifest[B], manifest[C], manifest[D], manifest[E],
            manifest[F], manifest[G], manifest[H], manifest[I], manifest[J],
            manifest[K], manifest[L]
        ) )

    /**
     * Builds a new bundle of the given type
     */
    def apply[
        A: Manifest, B: Manifest, C: Manifest, D: Manifest, E: Manifest,
        F: Manifest, G: Manifest, H: Manifest, I: Manifest, J: Manifest,
        K: Manifest, L: Manifest, M: Manifest
    ] ( callback: (
        Prereq with A with B with C with D with E with F with G with H with I
        with J with K with L with M
    ) => Response ): Handler
        = inner.build( callback, Set(
            manifest[A], manifest[B], manifest[C], manifest[D], manifest[E],
            manifest[F], manifest[G], manifest[H], manifest[I], manifest[J],
            manifest[K], manifest[L], manifest[M]
        ) )

    /**
     * Builds a new bundle of the given type
     */
    def apply[
        A: Manifest, B: Manifest, C: Manifest, D: Manifest, E: Manifest,
        F: Manifest, G: Manifest, H: Manifest, I: Manifest, J: Manifest,
        K: Manifest, L: Manifest, M: Manifest, N: Manifest
    ] ( callback: (
        Prereq with A with B with C with D with E with F with G with H with I
        with J with K with L with M with N
    ) => Response ): Handler
        = inner.build( callback, Set(
            manifest[A], manifest[B], manifest[C], manifest[D], manifest[E],
            manifest[F], manifest[G], manifest[H], manifest[I], manifest[J],
            manifest[K], manifest[L], manifest[M], manifest[N]
        ) )

    /**
     * Builds a new bundle of the given type
     */
    def apply[
        A: Manifest, B: Manifest, C: Manifest, D: Manifest, E: Manifest,
        F: Manifest, G: Manifest, H: Manifest, I: Manifest, J: Manifest,
        K: Manifest, L: Manifest, M: Manifest, N: Manifest, O: Manifest
    ] ( callback: (
        Prereq with A with B with C with D with E with F with G with H with I
        with J with K with L with M with N with O
    ) => Response ): Handler
        = inner.build( callback, Set(
            manifest[A], manifest[B], manifest[C], manifest[D], manifest[E],
            manifest[F], manifest[G], manifest[H], manifest[I], manifest[J],
            manifest[K], manifest[L], manifest[M], manifest[N], manifest[O]
        ) )

    /**
     * Builds a new bundle of the given type
     */
    def apply[
        A: Manifest, B: Manifest, C: Manifest, D: Manifest, E: Manifest,
        F: Manifest, G: Manifest, H: Manifest, I: Manifest, J: Manifest,
        K: Manifest, L: Manifest, M: Manifest, N: Manifest, O: Manifest,
        P: Manifest
    ] ( callback: (
        Prereq with A with B with C with D with E with F with G with H with I
        with J with K with L with M with N with O with P
    ) => Response ): Handler
        = inner.build( callback, Set(
            manifest[A], manifest[B], manifest[C], manifest[D], manifest[E],
            manifest[F], manifest[G], manifest[H], manifest[I], manifest[J],
            manifest[K], manifest[L], manifest[M], manifest[N], manifest[O],
            manifest[P]
        ) )

    /**
     * Builds a new bundle of the given type
     */
    def apply[
        A: Manifest, B: Manifest, C: Manifest, D: Manifest, E: Manifest,
        F: Manifest, G: Manifest, H: Manifest, I: Manifest, J: Manifest,
        K: Manifest, L: Manifest, M: Manifest, N: Manifest, O: Manifest,
        P: Manifest, Q: Manifest
    ] ( callback: (
        Prereq with A with B with C with D with E with F with G with H with I
        with J with K with L with M with N with O with P with Q
    ) => Response ): Handler
        = inner.build( callback, Set(
            manifest[A], manifest[B], manifest[C], manifest[D], manifest[E],
            manifest[F], manifest[G], manifest[H], manifest[I], manifest[J],
            manifest[K], manifest[L], manifest[M], manifest[N], manifest[O],
            manifest[P], manifest[Q]
        ) )

    /**
     * Builds a new bundle of the given type
     */
    def apply[
        A: Manifest, B: Manifest, C: Manifest, D: Manifest, E: Manifest,
        F: Manifest, G: Manifest, H: Manifest, I: Manifest, J: Manifest,
        K: Manifest, L: Manifest, M: Manifest, N: Manifest, O: Manifest,
        P: Manifest, Q: Manifest, R: Manifest
    ] ( callback: (
        Prereq with A with B with C with D with E with F with G with H with I
        with J with K with L with M with N with O with P with Q with R
    ) => Response ): Handler
        = inner.build( callback, Set(
            manifest[A], manifest[B], manifest[C], manifest[D], manifest[E],
            manifest[F], manifest[G], manifest[H], manifest[I], manifest[J],
            manifest[K], manifest[L], manifest[M], manifest[N], manifest[O],
            manifest[P], manifest[Q], manifest[R]
        ) )

    /**
     * Builds a new bundle of the given type
     */
    def apply[
        A: Manifest, B: Manifest, C: Manifest, D: Manifest, E: Manifest,
        F: Manifest, G: Manifest, H: Manifest, I: Manifest, J: Manifest,
        K: Manifest, L: Manifest, M: Manifest, N: Manifest, O: Manifest,
        P: Manifest, Q: Manifest, R: Manifest, S: Manifest
    ] ( callback: (
        Prereq with A with B with C with D with E with F with G with H with I
        with J with K with L with M with N with O with P with Q with R with S
    ) => Response ): Handler
        = inner.build( callback, Set(
            manifest[A], manifest[B], manifest[C], manifest[D], manifest[E],
            manifest[F], manifest[G], manifest[H], manifest[I], manifest[J],
            manifest[K], manifest[L], manifest[M], manifest[N], manifest[O],
            manifest[P], manifest[Q], manifest[R], manifest[S]
        ) )

    /**
     * Builds a new bundle of the given type
     */
    def apply[
        A: Manifest, B: Manifest, C: Manifest, D: Manifest, E: Manifest,
        F: Manifest, G: Manifest, H: Manifest, I: Manifest, J: Manifest,
        K: Manifest, L: Manifest, M: Manifest, N: Manifest, O: Manifest,
        P: Manifest, Q: Manifest, R: Manifest, S: Manifest, T: Manifest
    ] ( callback: (
        Prereq with A with B with C with D with E with F with G with H with I
        with J with K with L with M with N with O with P with Q with R with S
        with T
    ) => Response ): Handler
        = inner.build( callback, Set(
            manifest[A], manifest[B], manifest[C], manifest[D], manifest[E],
            manifest[F], manifest[G], manifest[H], manifest[I], manifest[J],
            manifest[K], manifest[L], manifest[M], manifest[N], manifest[O],
            manifest[P], manifest[Q], manifest[R], manifest[S], manifest[T]
        ) )

    /**
     * Builds a new bundle of the given type
     */
    def apply[
        A: Manifest, B: Manifest, C: Manifest, D: Manifest, E: Manifest,
        F: Manifest, G: Manifest, H: Manifest, I: Manifest, J: Manifest,
        K: Manifest, L: Manifest, M: Manifest, N: Manifest, O: Manifest,
        P: Manifest, Q: Manifest, R: Manifest, S: Manifest, T: Manifest,
        U: Manifest
    ] ( callback: (
        Prereq with A with B with C with D with E with F with G with H with I
        with J with K with L with M with N with O with P with Q with R with S
        with T with U
    ) => Response ): Handler
        = inner.build( callback, Set(
            manifest[A], manifest[B], manifest[C], manifest[D], manifest[E],
            manifest[F], manifest[G], manifest[H], manifest[I], manifest[J],
            manifest[K], manifest[L], manifest[M], manifest[N], manifest[O],
            manifest[P], manifest[Q], manifest[R], manifest[S], manifest[T],
            manifest[U]
        ) )

    /**
     * Builds a new bundle of the given type
     */
    def apply[
        A: Manifest, B: Manifest, C: Manifest, D: Manifest, E: Manifest,
        F: Manifest, G: Manifest, H: Manifest, I: Manifest, J: Manifest,
        K: Manifest, L: Manifest, M: Manifest, N: Manifest, O: Manifest,
        P: Manifest, Q: Manifest, R: Manifest, S: Manifest, T: Manifest,
        U: Manifest, V: Manifest
    ] ( callback: (
        Prereq with A with B with C with D with E with F with G with H with I
        with J with K with L with M with N with O with P with Q with R with S
        with T with U with V
    ) => Response ): Handler
        = inner.build( callback, Set(
            manifest[A], manifest[B], manifest[C], manifest[D], manifest[E],
            manifest[F], manifest[G], manifest[H], manifest[I], manifest[J],
            manifest[K], manifest[L], manifest[M], manifest[N], manifest[O],
            manifest[P], manifest[Q], manifest[R], manifest[S], manifest[T],
            manifest[U], manifest[V]
        ) )

    /**
     * Builds a new bundle of the given type
     */
    def apply[
        A: Manifest, B: Manifest, C: Manifest, D: Manifest, E: Manifest,
        F: Manifest, G: Manifest, H: Manifest, I: Manifest, J: Manifest,
        K: Manifest, L: Manifest, M: Manifest, N: Manifest, O: Manifest,
        P: Manifest, Q: Manifest, R: Manifest, S: Manifest, T: Manifest,
        U: Manifest, V: Manifest, W: Manifest
    ] ( callback: (
        Prereq with A with B with C with D with E with F with G with H with I
        with J with K with L with M with N with O with P with Q with R with S
        with T with U with V with W
    ) => Response ): Handler
        = inner.build( callback, Set(
            manifest[A], manifest[B], manifest[C], manifest[D], manifest[E],
            manifest[F], manifest[G], manifest[H], manifest[I], manifest[J],
            manifest[K], manifest[L], manifest[M], manifest[N], manifest[O],
            manifest[P], manifest[Q], manifest[R], manifest[S], manifest[T],
            manifest[U], manifest[V], manifest[W]
        ) )

    /**
     * Builds a new bundle of the given type
     */
    def apply[
        A: Manifest, B: Manifest, C: Manifest, D: Manifest, E: Manifest,
        F: Manifest, G: Manifest, H: Manifest, I: Manifest, J: Manifest,
        K: Manifest, L: Manifest, M: Manifest, N: Manifest, O: Manifest,
        P: Manifest, Q: Manifest, R: Manifest, S: Manifest, T: Manifest,
        U: Manifest, V: Manifest, W: Manifest, X: Manifest
    ] ( callback: (
        Prereq with A with B with C with D with E with F with G with H with I
        with J with K with L with M with N with O with P with Q with R with S
        with T with U with V with W with X
    ) => Response ): Handler
        = inner.build( callback, Set(
            manifest[A], manifest[B], manifest[C], manifest[D], manifest[E],
            manifest[F], manifest[G], manifest[H], manifest[I], manifest[J],
            manifest[K], manifest[L], manifest[M], manifest[N], manifest[O],
            manifest[P], manifest[Q], manifest[R], manifest[S], manifest[T],
            manifest[U], manifest[V], manifest[W], manifest[X]
        ) )

    /**
     * Builds a new bundle of the given type
     */
    def apply[
        A: Manifest, B: Manifest, C: Manifest, D: Manifest, E: Manifest,
        F: Manifest, G: Manifest, H: Manifest, I: Manifest, J: Manifest,
        K: Manifest, L: Manifest, M: Manifest, N: Manifest, O: Manifest,
        P: Manifest, Q: Manifest, R: Manifest, S: Manifest, T: Manifest,
        U: Manifest, V: Manifest, W: Manifest, X: Manifest, Y: Manifest
    ] ( callback: (
        Prereq with A with B with C with D with E with F with G with H with I
        with J with K with L with M with N with O with P with Q with R with S
        with T with U with V with W with X with Y
    ) => Response ): Handler
        = inner.build( callback, Set(
            manifest[A], manifest[B], manifest[C], manifest[D], manifest[E],
            manifest[F], manifest[G], manifest[H], manifest[I], manifest[J],
            manifest[K], manifest[L], manifest[M], manifest[N], manifest[O],
            manifest[P], manifest[Q], manifest[R], manifest[S], manifest[T],
            manifest[U], manifest[V], manifest[W], manifest[X], manifest[Y]
        ) )

    /**
     * Builds a new bundle of the given type
     */
    def apply[
        A: Manifest, B: Manifest, C: Manifest, D: Manifest, E: Manifest,
        F: Manifest, G: Manifest, H: Manifest, I: Manifest, J: Manifest,
        K: Manifest, L: Manifest, M: Manifest, N: Manifest, O: Manifest,
        P: Manifest, Q: Manifest, R: Manifest, S: Manifest, T: Manifest,
        U: Manifest, V: Manifest, W: Manifest, X: Manifest, Y: Manifest,
        Z: Manifest
    ] ( callback: (
        Prereq with A with B with C with D with E with F with G with H with I
        with J with K with L with M with N with O with P with Q with R with S
        with T with U with V with W with X with Y with Z
    ) => Response ): Handler
        = inner.build( callback, Set(
            manifest[A], manifest[B], manifest[C], manifest[D], manifest[E],
            manifest[F], manifest[G], manifest[H], manifest[I], manifest[J],
            manifest[K], manifest[L], manifest[M], manifest[N], manifest[O],
            manifest[P], manifest[Q], manifest[R], manifest[S], manifest[T],
            manifest[U], manifest[V], manifest[W], manifest[X], manifest[Y],
            manifest[Z]
        ) )

}

