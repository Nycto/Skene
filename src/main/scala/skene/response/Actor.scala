package com.roundeights.skene.response

import scala.actors.Actor

import com.roundeights.skene.Response
import com.roundeights.skene.Renderable

/**
 * A response that wraps an actor and sends it messages
 */
trait ActorResponse extends Response {

    /**
     * An actor that is used to communicate with the Response
     */
    protected def actor: Actor

    /** {@inheritDoc} */
    override def header( header: Response.Header ): Response = {
        actor ! header
        this
    }

    /** {@inheritDoc} */
    override def code ( code: Response.Code ): Response = {
        actor ! code
        this
    }

    /** {@inheritDoc} */
    override def content ( content: Renderable ): Response = {
        actor ! content
        this
    }

    /** {@inheritDoc} */
    override def flush (): Response = {
        actor ! Response.Flush()
        this
    }

    /** {@inheritDoc} */
    override def done (): Response = {
        actor ! Response.Done()
        this
    }

}


