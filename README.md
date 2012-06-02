Skene
=====

Skene is a Sinatra-esque HTTP request dispatching DSL. The goal is to make
it easy to build routing rules, read request data, and then return a response
in an immutable way.

[![Build Status](https://secure.travis-ci.org/Nycto/Skene.png?branch=master)](http://travis-ci.org/Nycto/Skene)


Basic Example
-------------

Putting to together a Servlet that works with Jetty or Tomcat is as easy as:

    package main.scala.com.example

    import org.skene._

    class BasicApp extends Skene {

        index {
            "<h1>Root Directory</h1>"
        }

        request("/url") { request =>
            "<h1>" + request.url + "</h1>"
        }

        request("/header") {
            Response("<h1>Custom Header Set</<h1>")
                .setHeader("X-Skene", "Interesting...")
        }

        default {
            Response(
                content = "<h1>404 not found</h1>",
                code = Response.NotFound
            )
        }
    }


