package it.unibo.pcd.akka.basics.e03state

import akka.actor.Status.Success
import akka.actor.typed.scaladsl.AskPattern.Askable
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}

import scala.io.StdIn.readLine

object GuessGame:
  // * passo anche a chi rispondere
  final case class Guess(number: Int, replyTo: ActorRef[GuessOutcome])

  sealed trait PlayerMessage
  case object NewInput extends PlayerMessage
  enum GuessOutcome extends PlayerMessage:
    // * tutti i possibili messaggi che possono essere inviati di tipo PlayerMessage
    case Guessed
    case Loss
    case NotGuessed(hint: Hint, remainingAttempts: Int)
  export GuessOutcome.*

  enum Hint:
    case TooSmall(attempt: Int)
    case TooBig(attempt: Int)
  export Hint.*

  object game:
    def apply(numberToGuess: Int, numberOfAttempts: Int = 10): Behavior[Guess] =
      // * Specifichiamo la logica di gestione dei messaggi
      Behaviors.receive { (context, msg: Guess) =>
        if (msg.number == numberToGuess)
          context.log.info(s"You guessed correctly my secret: $numberToGuess. Game ends here.")
          msg.replyTo ! Guessed
          Behaviors.stopped
        else
          context.log.info(s"Your guess is too ${if (msg.number < numberToGuess) "small" else "big"}.")
          val remainingAttempts = numberOfAttempts - 1
          msg.replyTo ! (
            if remainingAttempts <= 0 then Loss
            else
              NotGuessed(
                if (msg.number < numberToGuess) TooSmall(msg.number) else TooBig(msg.number),
                remainingAttempts
              )
          )
          if numberOfAttempts - 1 > 0 then game(numberToGuess, remainingAttempts)
          else
            context.log.info("You finished your attempts. Game ends here.")
            Behaviors.stopped
      }

  object player:
    def apply(
        game: ActorRef[Guess],
        guessLogic: Seq[Hint] => Int,
        hints: Seq[Hint] = Seq.empty
    ): Behavior[PlayerMessage] = Behaviors.receive[PlayerMessage] { (context, msg) =>
      msg match
        case NewInput =>
          val guess = guessLogic(hints)
          context.log.info(s"Trying with $guess")
          game ! GuessGame.Guess(guess, context.self)
          Behaviors.same
        case Guessed | Loss =>
          context.log.info("Done! Bye bye.")
          Behaviors.stopped
        case NotGuessed(hint, remainingAttempts) =>
          context.log.info(s"$hint.. Ouch! But I still have $remainingAttempts attempts..")
          context.self ! NewInput
          player(game, guessLogic, hints :+ hint)
    }

  def humanPlayer(game: ActorRef[Guess]): Behavior[PlayerMessage] =
    player(game, hints => readLine(s"Last hint: ${hints.lastOption}\nGuess: ").toInt)

  private case class BoundedInterval(lb: Int, ub: Int):
    require(lb <= ub)

  def randomPlayer(
      game: ActorRef[Guess],
      lb: Int = Int.MinValue,
      ub: Int = Int.MaxValue,
      seed: Int = 0
  ): Behavior[PlayerMessage] =
    var bounds = BoundedInterval(lb, ub)
    scala.util.Random.setSeed(seed)
    player(
      game,
      hints =>
        hints.lastOption.foreach {
          case TooSmall(attempt) => bounds = BoundedInterval(Math.max(bounds.lb, attempt + 1), bounds.ub)
          case TooBig(attempt) => bounds = BoundedInterval(bounds.lb, Math.min(bounds.ub, attempt - 1))
        }
        bounds.lb + scala.util.Random.nextInt(bounds.ub - bounds.lb + 1)
    )
end GuessGame

object GuessNumberMain extends App:
  case object StartPlay

  val system = ActorSystem(
    // * StartPlay è il messaggio che viene inviato al system per iniziare il gioco
    Behaviors.receive[StartPlay.type] { (context, _) =>
      context.log.info("Starting a game.")
      // * Creaiamo un attore con il Behavior specificato e il seguente nome (modella il gioco stesso)
      val game = context.spawn(GuessGame.game(scala.util.Random.nextInt(100), numberOfAttempts = 5), "guess-listener")
      // * Creiamo un attore che modella il giocatore
      val player = context.spawn(
        // GuessGame.humanPlayer(game),
        GuessGame.randomPlayer(game, 0, 100),
        "user"
      )
      player ! GuessGame.NewInput
      Behaviors.same
    },
    "hello-world-akka-system"
  )
  system ! StartPlay
