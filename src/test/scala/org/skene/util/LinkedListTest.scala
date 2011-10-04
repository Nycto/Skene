package test.scala.com.skene

import org.specs2.mutable._

import org.skene.util.LinkedList

class LinkedListTests extends Specification {

    val emptyList = LinkedList[String]()

    val fullList = LinkedList[String]().add("one").add("two").add("three")

    "The find method" should {

        "return None when the list is empty" in {
            emptyList.find( _ => Some(true) ) must_== None
        }

        "return None when the callback doesnt match anything" in {
            fullList.find( _ => None ) must_== None
        }

        "Return Some when the callback finds a match" in {
            fullList.find(
                str => if ( str.startsWith("o") ) Some(1) else None
            ) must_== Some(1)

            fullList.find(
                str => if ( str.startsWith("t") ) Some(2) else None
            ) must_== Some(2)

            fullList.find(
                str => if ( str.endsWith("ee") ) Some(3) else None
            ) must_== Some(3)
        }
    }
}

