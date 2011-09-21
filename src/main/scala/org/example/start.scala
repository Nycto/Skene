package main.scala.com.example

import org.skene.SkeneApp
import org.skene.Context

object Start extends SkeneApp( port = 8080 ) {

    index {
        "<h1>Root Directory</h1>"
    }

    request ("/one") {
        "<h1>One</<h1>"
    }

    request ("/two") {
        "<h1>Two</<h1>"
    }

    default {
        "<h1>404 not found</h1>"
    }
}

