package ch.usi.si.msde.edsl.assignment_03.model


/**
 * Just some context variables (givens) to enable async execution of http requests.
 */
object AsyncContext:
  import akka.actor.typed.ActorSystem
  import akka.actor.typed.scaladsl.Behaviors
  
  given system: ActorSystem[Nothing] = ActorSystem(Behaviors.empty, "SingleRequest")
  given ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global
end AsyncContext
