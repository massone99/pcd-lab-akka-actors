package it.unibo.pcd.akka.e01hello

import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import akka.actor.typed.scaladsl.Behaviors

object HelloBehavior {
  final case class Greet(whom: String, replyTo: ActorRef[Greeted])
  final case class Greeted(whom: String, from: ActorRef[Greet])

  def apply(): Behavior[Greet] = Behaviors.receive { (context, message) =>
    context.log.info("Hello {}!", message.whom)
    message.replyTo ! Greeted(message.whom, context.self)
    Behaviors.same
  }
}

object HelloWorldAkkaType extends App {
  val system: ActorSystem[HelloBehavior.Greet] = ActorSystem(guardianBehavior = HelloBehavior(), name = "hello-world-akka-system")
  system ! HelloBehavior.Greet("Akka Typed", system.ignoreRef)
  Thread.sleep(5000)
  system.terminate()
}