package main.scala.com.example

import org.skene._

object Start extends SkeneApp( port = 8080 ) {

    index {
        "<h1>Root Directory</h1>"
    }

    request ("/one") {
        "<h1>One</<h1>"
    }

    request ("/two") {
        Response("<h1>Two</<h1>")
            .setHeader("X-Skene", "Interesting...")
    }

    default {
        Response(
          content = "<h1>404 not found</h1>",
          code = Response.NotFound
        )
    }
}

