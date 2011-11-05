package test.scala.com.skene

import org.specs2.mutable._

import org.skene._

class SkeneTest extends Specification {

    val page = Response("page")
    val other = Response("other")

    "The index method" should {

        val interface = new Skene {
            index { page }
        }

        "apply to an absent path" in {
            val request = BareRequest( url = URL("http://example.com") )
            interface.handle(request) must_== page
        }

        "apply to an only a slash" in {
            val request = BareRequest( url = URL("http://example.com/") )
            interface.handle(request) must_== page
        }
    }

    "The request method" should {

        val interface = new Skene {
            request ("/one") { page }
            request ("/two") { other }
        }

        "match the path" in {
            val request = BareRequest(url = URL("http://example.com/one"))
            interface.handle(request) must_== page
        }

        "Ignore the request method" in {
            val request = BareRequest(
                url = URL("http://example.com/two"),
                method = Request.Method.POST()
            )
            interface.handle(request) must_== other
        }
    }

    "The default method" should {

        "be the fall back when nothing else matches" in {

            val interface = new Skene {
                index { other }
                default { page }
            }

            val request = BareRequest(url = URL("http://example.com/none"))

            interface.handle(request) must_== page
        }
    }

    "The method matchers" should {

        val getResponse = Response("get")
        val postResponse = Response("post")
        val deleteResponse = Response("delete")
        val putResponse = Response("put")

        val interface = new Skene {
            get ("/page") { getResponse }
            post ("/page") { postResponse }
            delete ("/page") { deleteResponse }
            put ("/page") { putResponse }
            default { other }
        }

        "match GET requests" in {
            val request = BareRequest(url = URL("http://example.com/page"))
            interface.handle(request) must_== getResponse
        }

        "match POST requests" in {
            val request = BareRequest(
                url = URL("http://example.com/page"),
                method = Request.Method.POST()
            )
            interface.handle(request) must_== postResponse
        }

        "match DELETE requests" in {
            val request = BareRequest(
                url = URL("http://example.com/page"),
                method = Request.Method.DELETE()
            )
            interface.handle(request) must_== deleteResponse
        }

        "match PUT requests" in {
            val request = BareRequest(
                url = URL("http://example.com/page"),
                method = Request.Method.PUT()
            )
            interface.handle(request) must_== putResponse
        }

        "not match the other request methods requests" in {
            val request = BareRequest(
                url = URL("http://example.com/page"),
                method = Request.Method.HEAD()
            )
            interface.handle(request) must_== other
        }
    }

    "The 'when' method" should {

        "match when the callback returns true" in {
            val interface = new Skene {
                when { req => true } { page }
            }
            interface.handle( BareRequest() ) must_== page
        }

        "be skipped when the callback returns false" in {
            val interface = new Skene {
                when { req => false } { other }
                default { page }
            }
            interface.handle( BareRequest() ) must_== page
        }

        "allow thunks to be passed in" in {
            val interface = new Skene {
                when { true } { page }
            }
            interface.handle( BareRequest() ) must_== page
        }

    }

}

