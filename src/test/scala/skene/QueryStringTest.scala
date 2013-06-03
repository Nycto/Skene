package test.scala.com.skene

import org.specs2.mutable._

import com.roundeights.skene.QueryString

class QueryStringTest extends Specification {

    "A QueryString" should {

        "Allow extra parameters to be added" in {
            ("a" -> "b") :: ("c" -> "d") :: QueryString() must_==
                QueryString( "a" -> "b", "c" -> "d" )
        }

        "Provide access to values by key" in {
            QueryString()("a") must_== None
            QueryString( "a" -> "b", "c" -> "d" )("c") must_== Some("d")
            QueryString( "a" -> "b", "a" -> "c" )("a") must_== Some("b")
        }

        "Generate URL encoded strings" in {
            QueryString().toString must_== ""
            QueryString( "a" -> "b", "c" -> "d" ).toString must_== "a=b&c=d"
            QueryString( "a&b" -> "c=d" ).toString must_== "a%26b=c%3Dd"
        }

        "Convert to a Map" in {
            QueryString().toMap must_== Map()
            QueryString( "a" -> "b", "c" -> "d" ).toMap must_==
                Map( "a" -> "b", "c" -> "d" )
        }

        "Group the same key if it appears multiple times" in {
            QueryString(
                "a" -> "1", "b" -> "2", "a" -> "3", "a" -> "3"
            ).grouped must_== Map(
                "a" -> Seq("1", "3", "3"), "b" -> Seq("2")
            )
        }

        "Return a list of keys" in {
            QueryString().keys must_== Set()
            QueryString( "a" -> "1", "b" -> "2" ).keys must_== Set("a", "b")
        }

        "Return a list of values" in {
            QueryString().values must_== Set()
            QueryString( "a" -> "1", "b" -> "2" ).values must_== Set("1", "2")
        }

        "Parse a string" in {
            QueryString("") must_== QueryString()
            QueryString("a=b") must_== QueryString("a" -> "b")
            QueryString("a=b&c=d") must_== QueryString("a" -> "b", "c" -> "d")
            QueryString("a=&=d") must_== QueryString("a" -> "", "" -> "d")
            QueryString("a==b") must_== QueryString("a" -> "=b")
            QueryString("a&&b") must_== QueryString("a" -> "", "b" -> "")
            QueryString("=&&=") must_== QueryString()
            QueryString("a%26b=c%3Dd") must_== QueryString( "a&b" -> "c=d" )
        }
    }

}

