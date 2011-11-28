package org.skene

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
    def response ( resp: Response ): Logger = {
        logger.info( resp.toString )
        this
    }

}


