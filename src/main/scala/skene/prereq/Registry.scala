package com.roundeights.skene

import scala.concurrent.{ExecutionContext, Promise}

/**
 * Companion for a Registry
 */
object Registry {

    /**
     * Thrown when attempting to use a Prereq without a registered provider
     */
    class UnregisteredPrereq ( missing: Class[_] ) extends Exception(
        "Attempting to use an unregistered Prereq: %s".format(
            missing.getSimpleName
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
    def apply()( implicit context: ExecutionContext ): Registry = new Registry

}


/**
 * A registry for collecting available Prerequisite Providers and combining
 * them into a Bundle
 */
class Registry private ( private val inner: RegistryData ) {

    /**
     * The public constructor
     */
    def this()( implicit context: ExecutionContext )
        = this( new RegistryData() )

    /**
     * Registers a new builder for a given type
     */
    def register[T: Manifest] ( builder: Provider[T] ): Registry
        = new Registry( inner.register(builder) )

    /**
     * Registers a callback to act as a Prereq provider
     */
    def register[T: Manifest] (
        builder: (Bundle, Promise[T]) => _,
        depends: Class[_]*
    ): Registry = new Registry( inner.register(builder, depends:_*) )

    /**
     * Registers a callback to act as a Prereq provider
     */
    def register[T: Manifest] (
        builder: ( Promise[T] ) => _,
        depends: Class[_]*
    ): Registry = new Registry( inner.register(builder, depends:_*) )

    /**
     * Returns the dependencies of a set of classes
     */
    def dependenciesOf ( clazzes: Class[_]* ): List[Class[_]]
        = inner.dependenciesOf( Set( clazzes: _* ) )

    /**
     * Builds a new bundle of the given type
     */
    def use[
        A: Manifest
    ]: Graph[
        Prereq with A
    ] = inner.build( new ClassList(
        manifest[A]
    ) )

    /**
     * Builds a new bundle of the given type
     */
    def use[
        A: Manifest, B: Manifest
    ]: Graph[
        Prereq with A with B
    ] = inner.build( new ClassList(
        manifest[A], manifest[B]
    ) )

    /**
     * Builds a new bundle of the given type
     */
    def use[
        A: Manifest, B: Manifest, C: Manifest
    ]: Graph[
        Prereq with A with B with C
    ] = inner.build( new ClassList(
        manifest[A], manifest[B], manifest[C]
    ) )

    /**
     * Builds a new bundle of the given type
     */
    def use[
        A: Manifest, B: Manifest, C: Manifest, D: Manifest
    ]: Graph[
        Prereq with A with B with C with D
    ] = inner.build( new ClassList(
        manifest[A], manifest[B], manifest[C], manifest[D]
    ) )

    /**
     * Builds a new bundle of the given type
     */
    def use[
        A: Manifest, B: Manifest, C: Manifest, D: Manifest, E: Manifest
    ]: Graph[
        Prereq with A with B with C with D with E
    ] = inner.build( new ClassList(
        manifest[A], manifest[B], manifest[C], manifest[D], manifest[E]
    ) )

    /**
     * Builds a new bundle of the given type
     */
    def use[
        A: Manifest, B: Manifest, C: Manifest, D: Manifest, E: Manifest,
        F: Manifest
    ]: Graph[
        Prereq with A with B with C with D with E with F
    ] = inner.build( new ClassList(
        manifest[A], manifest[B], manifest[C], manifest[D], manifest[E],
        manifest[F]
    ) )

    /**
     * Builds a new bundle of the given type
     */
    def use[
        A: Manifest, B: Manifest, C: Manifest, D: Manifest, E: Manifest,
        F: Manifest, G: Manifest
    ]: Graph[
        Prereq with A with B with C with D with E with F with G
    ] = inner.build( new ClassList(
        manifest[A], manifest[B], manifest[C], manifest[D], manifest[E],
        manifest[F], manifest[G]
    ) )

    /**
     * Builds a new bundle of the given type
     */
    def use[
        A: Manifest, B: Manifest, C: Manifest, D: Manifest, E: Manifest,
        F: Manifest, G: Manifest, H: Manifest
    ]: Graph[
        Prereq with A with B with C with D with E with F with G with H
    ] = inner.build( new ClassList(
        manifest[A], manifest[B], manifest[C], manifest[D], manifest[E],
        manifest[F], manifest[G], manifest[H]
    ) )

    /**
     * Builds a new bundle of the given type
     */
    def use[
        A: Manifest, B: Manifest, C: Manifest, D: Manifest, E: Manifest,
        F: Manifest, G: Manifest, H: Manifest, I: Manifest
    ]: Graph[
        Prereq with A with B with C with D with E with F with G with H with I
    ] = inner.build( new ClassList(
        manifest[A], manifest[B], manifest[C], manifest[D], manifest[E],
        manifest[F], manifest[G], manifest[H], manifest[I]
    ) )

    /**
     * Builds a new bundle of the given type
     */
    def use[
        A: Manifest, B: Manifest, C: Manifest, D: Manifest, E: Manifest,
        F: Manifest, G: Manifest, H: Manifest, I: Manifest, J: Manifest
    ]: Graph[
        Prereq with A with B with C with D with E with F with G with H with I
        with J
    ] = inner.build( new ClassList(
        manifest[A], manifest[B], manifest[C], manifest[D], manifest[E],
        manifest[F], manifest[G], manifest[H], manifest[I], manifest[J]
    ) )

    /**
     * Builds a new bundle of the given type
     */
    def use[
        A: Manifest, B: Manifest, C: Manifest, D: Manifest, E: Manifest,
        F: Manifest, G: Manifest, H: Manifest, I: Manifest, J: Manifest,
        K: Manifest
    ]: Graph[
        Prereq with A with B with C with D with E with F with G with H with I
        with J with K
    ] = inner.build( new ClassList(
        manifest[A], manifest[B], manifest[C], manifest[D], manifest[E],
        manifest[F], manifest[G], manifest[H], manifest[I], manifest[J],
        manifest[K]
    ) )

    /**
     * Builds a new bundle of the given type
     */
    def use[
        A: Manifest, B: Manifest, C: Manifest, D: Manifest, E: Manifest,
        F: Manifest, G: Manifest, H: Manifest, I: Manifest, J: Manifest,
        K: Manifest, L: Manifest
    ]: Graph[
        Prereq with A with B with C with D with E with F with G with H with I
        with J with K with L
    ] = inner.build( new ClassList(
        manifest[A], manifest[B], manifest[C], manifest[D], manifest[E],
        manifest[F], manifest[G], manifest[H], manifest[I], manifest[J],
        manifest[K], manifest[L]
    ) )

    /**
     * Builds a new bundle of the given type
     */
    def use[
        A: Manifest, B: Manifest, C: Manifest, D: Manifest, E: Manifest,
        F: Manifest, G: Manifest, H: Manifest, I: Manifest, J: Manifest,
        K: Manifest, L: Manifest, M: Manifest
    ]: Graph[
        Prereq with A with B with C with D with E with F with G with H with I
        with J with K with L with M
    ] = inner.build( new ClassList(
        manifest[A], manifest[B], manifest[C], manifest[D], manifest[E],
        manifest[F], manifest[G], manifest[H], manifest[I], manifest[J],
        manifest[K], manifest[L], manifest[M]
    ) )

    /**
     * Builds a new bundle of the given type
     */
    def use[
        A: Manifest, B: Manifest, C: Manifest, D: Manifest, E: Manifest,
        F: Manifest, G: Manifest, H: Manifest, I: Manifest, J: Manifest,
        K: Manifest, L: Manifest, M: Manifest, N: Manifest
    ]: Graph[
        Prereq with A with B with C with D with E with F with G with H with I
        with J with K with L with M with N
    ] = inner.build( new ClassList(
        manifest[A], manifest[B], manifest[C], manifest[D], manifest[E],
        manifest[F], manifest[G], manifest[H], manifest[I], manifest[J],
        manifest[K], manifest[L], manifest[M], manifest[N]
    ) )

    /**
     * Builds a new bundle of the given type
     */
    def use[
        A: Manifest, B: Manifest, C: Manifest, D: Manifest, E: Manifest,
        F: Manifest, G: Manifest, H: Manifest, I: Manifest, J: Manifest,
        K: Manifest, L: Manifest, M: Manifest, N: Manifest, O: Manifest
    ]: Graph[
        Prereq with A with B with C with D with E with F with G with H with I
        with J with K with L with M with N with O
    ] = inner.build( new ClassList(
        manifest[A], manifest[B], manifest[C], manifest[D], manifest[E],
        manifest[F], manifest[G], manifest[H], manifest[I], manifest[J],
        manifest[K], manifest[L], manifest[M], manifest[N], manifest[O]
    ) )

    /**
     * Builds a new bundle of the given type
     */
    def use[
        A: Manifest, B: Manifest, C: Manifest, D: Manifest, E: Manifest,
        F: Manifest, G: Manifest, H: Manifest, I: Manifest, J: Manifest,
        K: Manifest, L: Manifest, M: Manifest, N: Manifest, O: Manifest,
        P: Manifest
    ]: Graph[
        Prereq with A with B with C with D with E with F with G with H with I
        with J with K with L with M with N with O with P
    ] = inner.build( new ClassList(
        manifest[A], manifest[B], manifest[C], manifest[D], manifest[E],
        manifest[F], manifest[G], manifest[H], manifest[I], manifest[J],
        manifest[K], manifest[L], manifest[M], manifest[N], manifest[O],
        manifest[P]
    ) )

    /**
     * Builds a new bundle of the given type
     */
    def use[
        A: Manifest, B: Manifest, C: Manifest, D: Manifest, E: Manifest,
        F: Manifest, G: Manifest, H: Manifest, I: Manifest, J: Manifest,
        K: Manifest, L: Manifest, M: Manifest, N: Manifest, O: Manifest,
        P: Manifest, Q: Manifest
    ]: Graph[
        Prereq with A with B with C with D with E with F with G with H with I
        with J with K with L with M with N with O with P with Q
    ] = inner.build( new ClassList(
        manifest[A], manifest[B], manifest[C], manifest[D], manifest[E],
        manifest[F], manifest[G], manifest[H], manifest[I], manifest[J],
        manifest[K], manifest[L], manifest[M], manifest[N], manifest[O],
        manifest[P], manifest[Q]
    ) )

    /**
     * Builds a new bundle of the given type
     */
    def use[
        A: Manifest, B: Manifest, C: Manifest, D: Manifest, E: Manifest,
        F: Manifest, G: Manifest, H: Manifest, I: Manifest, J: Manifest,
        K: Manifest, L: Manifest, M: Manifest, N: Manifest, O: Manifest,
        P: Manifest, Q: Manifest, R: Manifest
    ]: Graph[
        Prereq with A with B with C with D with E with F with G with H with I
        with J with K with L with M with N with O with P with Q with R
    ] = inner.build( new ClassList(
        manifest[A], manifest[B], manifest[C], manifest[D], manifest[E],
        manifest[F], manifest[G], manifest[H], manifest[I], manifest[J],
        manifest[K], manifest[L], manifest[M], manifest[N], manifest[O],
        manifest[P], manifest[Q], manifest[R]
    ) )

    /**
     * Builds a new bundle of the given type
     */
    def use[
        A: Manifest, B: Manifest, C: Manifest, D: Manifest, E: Manifest,
        F: Manifest, G: Manifest, H: Manifest, I: Manifest, J: Manifest,
        K: Manifest, L: Manifest, M: Manifest, N: Manifest, O: Manifest,
        P: Manifest, Q: Manifest, R: Manifest, S: Manifest
    ]: Graph[
        Prereq with A with B with C with D with E with F with G with H with I
        with J with K with L with M with N with O with P with Q with R with S
    ] = inner.build( new ClassList(
        manifest[A], manifest[B], manifest[C], manifest[D], manifest[E],
        manifest[F], manifest[G], manifest[H], manifest[I], manifest[J],
        manifest[K], manifest[L], manifest[M], manifest[N], manifest[O],
        manifest[P], manifest[Q], manifest[R], manifest[S]
    ) )

    /**
     * Builds a new bundle of the given type
     */
    def use[
        A: Manifest, B: Manifest, C: Manifest, D: Manifest, E: Manifest,
        F: Manifest, G: Manifest, H: Manifest, I: Manifest, J: Manifest,
        K: Manifest, L: Manifest, M: Manifest, N: Manifest, O: Manifest,
        P: Manifest, Q: Manifest, R: Manifest, S: Manifest, T: Manifest
    ]: Graph[
        Prereq with A with B with C with D with E with F with G with H with I
        with J with K with L with M with N with O with P with Q with R with S
        with T
    ] = inner.build( new ClassList(
        manifest[A], manifest[B], manifest[C], manifest[D], manifest[E],
        manifest[F], manifest[G], manifest[H], manifest[I], manifest[J],
        manifest[K], manifest[L], manifest[M], manifest[N], manifest[O],
        manifest[P], manifest[Q], manifest[R], manifest[S], manifest[T]
    ) )

    /**
     * Builds a new bundle of the given type
     */
    def use[
        A: Manifest, B: Manifest, C: Manifest, D: Manifest, E: Manifest,
        F: Manifest, G: Manifest, H: Manifest, I: Manifest, J: Manifest,
        K: Manifest, L: Manifest, M: Manifest, N: Manifest, O: Manifest,
        P: Manifest, Q: Manifest, R: Manifest, S: Manifest, T: Manifest,
        U: Manifest
    ]: Graph[
        Prereq with A with B with C with D with E with F with G with H with I
        with J with K with L with M with N with O with P with Q with R with S
        with T with U
    ] = inner.build( new ClassList(
        manifest[A], manifest[B], manifest[C], manifest[D], manifest[E],
        manifest[F], manifest[G], manifest[H], manifest[I], manifest[J],
        manifest[K], manifest[L], manifest[M], manifest[N], manifest[O],
        manifest[P], manifest[Q], manifest[R], manifest[S], manifest[T],
        manifest[U]
    ) )

    /**
     * Builds a new bundle of the given type
     */
    def use[
        A: Manifest, B: Manifest, C: Manifest, D: Manifest, E: Manifest,
        F: Manifest, G: Manifest, H: Manifest, I: Manifest, J: Manifest,
        K: Manifest, L: Manifest, M: Manifest, N: Manifest, O: Manifest,
        P: Manifest, Q: Manifest, R: Manifest, S: Manifest, T: Manifest,
        U: Manifest, V: Manifest
    ]: Graph[
        Prereq with A with B with C with D with E with F with G with H with I
        with J with K with L with M with N with O with P with Q with R with S
        with T with U with V
    ] = inner.build( new ClassList(
        manifest[A], manifest[B], manifest[C], manifest[D], manifest[E],
        manifest[F], manifest[G], manifest[H], manifest[I], manifest[J],
        manifest[K], manifest[L], manifest[M], manifest[N], manifest[O],
        manifest[P], manifest[Q], manifest[R], manifest[S], manifest[T],
        manifest[U], manifest[V]
    ) )

    /**
     * Builds a new bundle of the given type
     */
    def use[
        A: Manifest, B: Manifest, C: Manifest, D: Manifest, E: Manifest,
        F: Manifest, G: Manifest, H: Manifest, I: Manifest, J: Manifest,
        K: Manifest, L: Manifest, M: Manifest, N: Manifest, O: Manifest,
        P: Manifest, Q: Manifest, R: Manifest, S: Manifest, T: Manifest,
        U: Manifest, V: Manifest, W: Manifest
    ]: Graph[
        Prereq with A with B with C with D with E with F with G with H with I
        with J with K with L with M with N with O with P with Q with R with S
        with T with U with V with W
    ] = inner.build( new ClassList(
        manifest[A], manifest[B], manifest[C], manifest[D], manifest[E],
        manifest[F], manifest[G], manifest[H], manifest[I], manifest[J],
        manifest[K], manifest[L], manifest[M], manifest[N], manifest[O],
        manifest[P], manifest[Q], manifest[R], manifest[S], manifest[T],
        manifest[U], manifest[V], manifest[W]
    ) )

    /**
     * Builds a new bundle of the given type
     */
    def use[
        A: Manifest, B: Manifest, C: Manifest, D: Manifest, E: Manifest,
        F: Manifest, G: Manifest, H: Manifest, I: Manifest, J: Manifest,
        K: Manifest, L: Manifest, M: Manifest, N: Manifest, O: Manifest,
        P: Manifest, Q: Manifest, R: Manifest, S: Manifest, T: Manifest,
        U: Manifest, V: Manifest, W: Manifest, X: Manifest
    ]: Graph[
        Prereq with A with B with C with D with E with F with G with H with I
        with J with K with L with M with N with O with P with Q with R with S
        with T with U with V with W with X
    ] = inner.build( new ClassList(
        manifest[A], manifest[B], manifest[C], manifest[D], manifest[E],
        manifest[F], manifest[G], manifest[H], manifest[I], manifest[J],
        manifest[K], manifest[L], manifest[M], manifest[N], manifest[O],
        manifest[P], manifest[Q], manifest[R], manifest[S], manifest[T],
        manifest[U], manifest[V], manifest[W], manifest[X]
    ) )

    /**
     * Builds a new bundle of the given type
     */
    def use[
        A: Manifest, B: Manifest, C: Manifest, D: Manifest, E: Manifest,
        F: Manifest, G: Manifest, H: Manifest, I: Manifest, J: Manifest,
        K: Manifest, L: Manifest, M: Manifest, N: Manifest, O: Manifest,
        P: Manifest, Q: Manifest, R: Manifest, S: Manifest, T: Manifest,
        U: Manifest, V: Manifest, W: Manifest, X: Manifest, Y: Manifest
    ]: Graph[
        Prereq with A with B with C with D with E with F with G with H with I
        with J with K with L with M with N with O with P with Q with R with S
        with T with U with V with W with X with Y
    ] = inner.build( new ClassList(
        manifest[A], manifest[B], manifest[C], manifest[D], manifest[E],
        manifest[F], manifest[G], manifest[H], manifest[I], manifest[J],
        manifest[K], manifest[L], manifest[M], manifest[N], manifest[O],
        manifest[P], manifest[Q], manifest[R], manifest[S], manifest[T],
        manifest[U], manifest[V], manifest[W], manifest[X], manifest[Y]
    ) )

    /**
     * Builds a new bundle of the given type
     */
    def use[
        A: Manifest, B: Manifest, C: Manifest, D: Manifest, E: Manifest,
        F: Manifest, G: Manifest, H: Manifest, I: Manifest, J: Manifest,
        K: Manifest, L: Manifest, M: Manifest, N: Manifest, O: Manifest,
        P: Manifest, Q: Manifest, R: Manifest, S: Manifest, T: Manifest,
        U: Manifest, V: Manifest, W: Manifest, X: Manifest, Y: Manifest,
        Z: Manifest
    ]: Graph[
        Prereq with A with B with C with D with E with F with G with H with I
        with J with K with L with M with N with O with P with Q with R with S
        with T with U with V with W with X with Y with Z
    ] = inner.build( new ClassList(
        manifest[A], manifest[B], manifest[C], manifest[D], manifest[E],
        manifest[F], manifest[G], manifest[H], manifest[I], manifest[J],
        manifest[K], manifest[L], manifest[M], manifest[N], manifest[O],
        manifest[P], manifest[Q], manifest[R], manifest[S], manifest[T],
        manifest[U], manifest[V], manifest[W], manifest[X], manifest[Y],
        manifest[Z]
    ) )

}
