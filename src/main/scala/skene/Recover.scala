package com.roundeights.skene


/**
 * Allows for recovery when executing code that could throw an exception.
 */
class Recover ( logger: Logger ) {

    /**
     * Executes the given thunk and recovers if it throws an exception
     */
    def from ( response: Response )( action: () => Unit ): Unit = {
        try {
            action()
        } catch { case err: Throwable => {
            logger.error( err )

            response.serverError.html(
                <html>
                    <head><title>500 Internal Server Error</title></head>
                    <body><h1>500 Internal Server Error</h1></body>
                </html>
            ).done()

        } }
    }

}

