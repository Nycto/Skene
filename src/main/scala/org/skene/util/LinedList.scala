package org.skene.util

/**
 * Companion class
 */
object LinkedList {

    /**
     * Creates a new list
     */
    def apply[T] () = new LinkedList[T]
}

/**
 * A simple, thread safe linked list
 */
class LinkedList[T] {

    /**
     * A single link in the list
     */
    private sealed abstract class Link[T] {
        /**
         * Applies a callback to each link and returns the first value that
         * isn't None
         */
        def find[U] ( callback: (T) => Option[U] ): Option[U]
    }

    /**
     * A terminal link
     */
    private case class NoLink[T] () extends Link[T] {
        /**
         * @see Link
         */
        def find[U] ( callback: (T) => Option[U] ): Option[U] = None
    }

    /**
     * A concrete link
     */
    private case class SomeLink[T] ( val value: T ) extends Link[T] {
        /**
         * The next link in the chain
         */
        var next: Link[T] = new NoLink

        /**
         * @see Link
         */
        def find[U] ( callback: (T) => Option[U] ): Option[U] = {
            callback(value) match {
                case None => next.find( callback )
                case result @ Some(_) => result
            }
        }
    }

    /**
     * The first link in the list
     */
    private var first: Link[T] = new NoLink

    /**
     * The last link in the list
     */
    private var last: Option[SomeLink[T]] = None

    /**
     * Adds a new entry to this list
     */
    def add ( value: T ): LinkedList[T] = synchronized {
        val nextLink = SomeLink( value )

        last match {
            case Some( link ) => link.next = nextLink
            case None => first = nextLink
        }

        last = Some( nextLink )

        this
    }

    /**
     * Searches for the first link in this list that returns some value
     * from a callback
     */
    def find[U] ( callback: (T) => Option[U] ): Option[U] = first.find(callback)
}

