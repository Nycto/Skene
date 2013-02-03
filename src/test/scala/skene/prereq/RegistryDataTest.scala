package com.roundeights.skene

import org.specs2.mutable._
import org.specs2.mock._

import scala.concurrent._
import scala.concurrent.duration._
import java.util.concurrent.Executor

class RegistryDataTest extends Specification with Mockito {

    /** An execution context that runs in the calling thread */
    implicit val context = ExecutionContext.fromExecutor(new Executor {
        override def execute( command: Runnable ): Unit = command.run
    })


    trait Req1 { def one: String }
    trait Req2 { def two: String }
    trait Req3 { def three: String }
    trait Req3Conflict { def three: String }
    trait Req4 { def four: String }
    trait Req5 { def five: String }
    trait Req6 { override def toString: String = "Req6" }
    trait Req7 { override def toString: String = "Req7" }


    private val registry = new RegistryData()
        .register[Req1](
            (_: Promise[Req1]) => (),
            classOf[Req2], classOf[Req3]
        )
        .register[Req2]( (_: Promise[Req2]) => (), classOf[Req3] )
        .register[Req3]( (_: Promise[Req3]) => () )
        .register[Req3Conflict]( (_: Promise[Req3Conflict]) => () )
        .register[Req4]( (_: Promise[Req4]) => (), classOf[Req5] )
        .register[Req6]( (_: Promise[Req6]) => () )
        .register[Req7]( (_: Promise[Req7]) => () )


    "The dependency manager" should {

        "linearize a dependency tree" in {
            registry.dependenciesOf( Set(classOf[Req1]) ) must_== List(
                classOf[Req3], classOf[Req2], classOf[Req1]
            )

            registry.dependenciesOf( Set(classOf[Req2]) ) must_== List(
                classOf[Req3], classOf[Req2]
            )

            registry.dependenciesOf( Set(classOf[Req3]) ) must_== List(
                classOf[Req3]
            )
        }

        "throw when a provider doesn't exist" in {
            registry.dependenciesOf( Set(classOf[Req5]) ) must throwA[
                Registry.UnregisteredPrereq
            ]

            registry.dependenciesOf( Set(classOf[Req4]) ) must throwA[
                Registry.UnregisteredPrereq
            ]
        }

        "throw when two prereqs have conflicting interfaces" in {
            registry.checkConflicts( List(
                classOf[Req3], classOf[Req3Conflict]
            ) ) must throwA[ Registry.ConflictingPrereqs ]

            registry.checkConflicts( List(
                classOf[Req1], classOf[Req2], classOf[Req3]
            ) )

            ok
        }

        "ignore toString methods when examining conflicting interfaces" in {
            registry.checkConflicts( List(
                classOf[Req6], classOf[Req7]
            ) )

            ok
        }

    }

}


