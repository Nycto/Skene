package com.roundeights.skene

import org.slf4j.{Logger => Slf4jLogger,LoggerFactory}

object Logger {

    /**
     * The shared default slf4j logger
     */
    private lazy val internal: Slf4jLogger = LoggerFactory.getLogger(getClass)

    /**
     * The shared default logger
     */
    val logger = new Logger

    /**
     * An null logger
     */
    lazy val nil = new Logger {
        override def request ( req: Request ): Logger = this
        override def response (
            requestID: Long, status: Response.Code
        ): Logger = this
        override def error ( err: Throwable ): Logger = this
    }
}

/**
 * A helper class for logging Skene actions.
 */
class Logger ( private val logger: Slf4jLogger = Logger.internal ) {

    /**
     * Logs a skene request
     */
    def request ( req: Request ): Logger = {
        logger.info( req.toString )
        this
    }

    /**
     * Logs a skene response
     */
    def response ( requestID: Long, status: Response.Code ): Logger = {
        logger.info( "[Response #%d %s]".format( requestID, status ) )
        this
    }

    /**
     * Logs an exception
     */
    def error ( err: Throwable ): Logger = {
        logger.error(List(
            err.toString,
            err.getStackTrace.map( _.toString ).mkString("; ")
        ).mkString(" "))
        this
    }

    /**
     * Logs an error message
     */
    def errorMsg ( requestID: Long, err: Throwable ): Logger = {
        logger.error("[Request #%d Error: %s]".format(requestID, err.toString))
        this
    }

}


