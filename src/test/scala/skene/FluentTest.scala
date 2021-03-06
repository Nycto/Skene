package test.scala.com.skene

import org.specs2.mutable._
import org.specs2.mock._

import com.roundeights.skene._
import scala.concurrent.ExecutionContext.Implicits.global


class FluentTest extends Specification with Mockito {

    /**
     * A helper for asserting that a handler will be called for a given URL
     */
    class AssertURL ( val request: Request )  {

        def this ( url: String ) = this( BareRequest( url = URL(url) ) )

        val recover = Recover.using { case err: Throwable => throw err }

        val response = mock[Response]
        val pass = mock[Handler]
        val fail = mock[Handler]

        def matches ( interface: Handler ) = {
            interface.handle( recover, request, response )
            there was no(fail).handle( recover, request, response )
            there was one(pass).handle( recover, request, response )
        }
    }


    "The index method" should {

        "apply to an absent path" in {
            val test = new AssertURL("http://example.com")
            test.matches( new Skene {
                index { test.pass }
                default { test.fail }
            } )
        }

        "apply when there is only a slash" in {
            val test = new AssertURL("http://example.com/")
            test.matches( new Skene {
                index { test.pass }
                default { test.fail }
            } )
        }
    }

    "The request method" should {

        "match the path" in {
            val test = new AssertURL("http://example.com/one")
            test.matches( new Skene {
                request ("/one") { test.pass }
                request ("/two") { test.fail }
            } )
        }

        "Ignore the request method" in {
            val test = new AssertURL("http://example.com/two")
            test.matches( new Skene {
                request ("/one") { test.fail }
                request ("/two") { test.pass }
                default { test.fail }
            } )
        }
    }

    "The default method" should {

        "be the fall back when nothing else matches" in {
            val test = new AssertURL("http://example.com/none")
            test.matches( new Skene {
                index { test.fail }
                request ("/one") { test.fail }
                request ("/two") { test.fail }
                default { test.pass }
            } )
        }
    }

    "The method matchers" should {

        "match GET requests" in {
            val test = new AssertURL("http://example.com/page")
            test.matches( new Skene {
                get("/page") { test.pass }
                post("/page") { test.fail }
                delete("/page") { test.fail }
                put("/page") { test.fail }
                default { test.fail }
            } )
        }

        "match POST requests" in {
            val test = new AssertURL(BareRequest(
                url = URL("http://example.com/page"),
                method = Request.Method.POST
            ))

            test.matches( new Skene {
                get("/page") { test.fail }
                post("/page") { test.pass }
                delete("/page") { test.fail }
                put("/page") { test.fail }
                default { test.fail }
            } )
        }

        "match DELETE requests" in {
            val test = new AssertURL(BareRequest(
                url = URL("http://example.com/page"),
                method = Request.Method.DELETE
            ))

            test.matches( new Skene {
                get("/page") { test.fail }
                post("/page") { test.fail }
                delete("/page") { test.pass }
                put("/page") { test.fail }
                default { test.fail }
            } )
        }

        "match PUT requests" in {
            val test = new AssertURL(BareRequest(
                url = URL("http://example.com/page"),
                method = Request.Method.PUT
            ))

            test.matches( new Skene {
                get("/page") { test.fail }
                post("/page") { test.fail }
                delete("/page") { test.fail }
                put("/page") { test.pass }
                default { test.fail }
            } )
        }

        "match HEAD requests" in {
            val test = new AssertURL(BareRequest(
                url = URL("http://example.com/page"),
                method = Request.Method.HEAD
            ))

            test.matches( new Skene {
                get("/page") { test.fail }
                post("/page") { test.fail }
                delete("/page") { test.fail }
                put("/page") { test.fail }
                default { test.pass }
            } )
        }
    }

    "The 'when' method" should {

        "match when the callback returns true" in {
            val test = new AssertURL( BareRequest() )
            test.matches( new Skene {
                when { req => true } { test.pass }
                default { test.fail }
            } )
        }

        "be skipped when the callback returns false" in {
            val test = new AssertURL( BareRequest() )
            test.matches( new Skene {
                when { req => false } { test.fail }
                default { test.pass }
            } )
        }

        "allow thunks to be passed in" in {
            val test = new AssertURL( BareRequest() )
            test.matches( new Skene {
                when { true } { test.pass }
                default { test.fail }
            } )
        }
    }

    "The 'and' operator" should {

        "pass when both operands pass" in {
            val test = new AssertURL( BareRequest() )
            test.matches( new Skene {
                when( true ) and when( true ) apply { test.pass }
                default { test.fail }
            } )
        }

        "be skipped when either operand fail" in {
            val test = new AssertURL( BareRequest() )
            test.matches( new Skene {
                when( true ) and when( false ) apply { test.fail }
                default { test.pass }
            } )
        }

        "allow for explicit dot notation" in {
            val test = new AssertURL( BareRequest() )
            test.matches( new Skene {
                when( true ).and.when( true ) { test.pass }
                default { test.fail }
            } )
        }
    }

    "The 'or' operator" should {

        "pass when either operand pass" in {
            val test = new AssertURL( BareRequest() )
            test.matches( new Skene {
                when( true ) or when( false ) apply { test.pass }
                default { test.fail }
            } )
        }

        "be skipped when both operands fail" in {
            val test = new AssertURL( BareRequest() )
            test.matches( new Skene {
                when( false ) or when( false ) apply { test.fail }
                default { test.pass }
            } )
        }

        "allow for explicit dot notation" in {
            val test = new AssertURL( BareRequest() )
            test.matches( new Skene {
                when( false ).or.when( true ) { test.pass }
                default { test.fail }
            } )
        }
    }

    "The 'not' operator" should {

        "pass when then inner operation fails" in {
            val test = new AssertURL( BareRequest() )
            test.matches( new Skene {
                not( when(false) ) apply { test.pass }
                default { test.fail }
            } )
        }

        "be skipped when the inner operation passes" in {
            val test = new AssertURL( BareRequest() )
            test.matches( new Skene {
                not( when(true) ) apply { test.fail }
                default { test.pass }
            } )
        }

        "allow for explicit dot notation" in {
            val test = new AssertURL( BareRequest() )
            test.matches( new Skene {
                not.when(false) { test.pass }
                default { test.fail }
            } )
        }
    }

    "The 'delegate' method" should {

        "Hand the request to a nested interface" in {
            val test = new AssertURL( BareRequest() )
            test.matches( new Skene {
                delegate( new Skene {
                    when( true ) { test.pass }
                })
                default { test.fail }
            } )
        }
    }

}

