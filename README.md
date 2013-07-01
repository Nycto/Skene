Skene [![Build Status](https://secure.travis-ci.org/Nycto/Skene.png?branch=master)](http://travis-ci.org/Nycto/Skene)
=====

Skene is a non-block HTTP request dispatcher for Scala. It lets you quickly
build simple web apps without encorcing any strict restrictions on your project.

Inspiration was taken from Sinatra, NodeJS, and a touch from Swing. Skene aims
to make it easy to build routing rules, read request data, and then return a
response.


Basic Example
-------------

Putting to together a Servlet that works with Jetty or Tomcat is as easy as:

```scala
package com.example

import com.roundeights.skene._
import scala.concurrent.ExecutionContext.Implicits.global

class BasicServlet extends Skene {

    index( (request: Request, response: Response) => {
        response.html(
            "<html><body><h1>Root Directory</h1></body></html>"
        ).done
    })

    get("/url")( (request: Request, response: Response) => {
        response.html(
            "<html><body><h1>" + request.url + "</h1></body></html>"
        ).done
    })

    default( (request: Request, response: Response) => {
        response.html(
            "<html><body><h1>404 Not Found</h1></body></html>"
        ).done
    })

}
```

You then just need to update your servlet config like this:

* src/main/webapp/WEB-INF/web.xml

  ```xml
  <!DOCTYPE web-app PUBLIC
  "-//Sun Microsystems, Inc.//DTD Web Application 2.3//EN"
  "http://java.sun.com/dtd/web-app_2_3.dtd" >

  <web-app>
      <display-name>MyApp</display-name>

      <servlet>
          <servlet-name>Site</servlet-name>
          <servlet-class>com.example.BasicServlet</servlet-class>
          <load-on-startup>1</load-on-startup>
      </servlet>

      <servlet-mapping>
          <servlet-name>Site</servlet-name>
          <url-pattern>/*</url-pattern>
      </servlet-mapping>

  </web-app>
  ```


