package org.skene

/**
 * Represents the context of a request and the associated response
 */
trait Context {

    /**
     * The requested URL
     */
    def url: URL;
}


