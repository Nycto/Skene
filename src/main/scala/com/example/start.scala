package main.scala.com.example

import main.scala.com.skene._

object Start {
    def main(args : Array[String]): Unit = {
        Server(
            port = 8080,
            handler = new Dispatcher
                add( Matcher.path("/one"), Handler {
                    <h1>Path One</h1>
                })
                add( Matcher.path("/two"), Handler {
                    <h1>Path Two</h1>
                })
        )
    }
}

