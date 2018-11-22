package marsroverkata

import cats.effect._
import cats.implicits._

import scala.io.StdIn._
import scala.util.Try

class Game {

  def puts(s: String): IO[Unit] = IO(println(s))

  def ask(msg: String): IO[String] =
    puts(msg)
      .flatMap(_ => reads())

  def reads(): IO[String] = IO(readLine())

  case class Coordinates(x: Int, y: Int)

  case class Dimension(w: Int, h: Int)

  sealed trait Direction

  case object N extends Direction

  case object E extends Direction

  case object S extends Direction

  case object W extends Direction

  case class Position(coordinates: Coordinates, direction: Direction)

  sealed trait Command

  sealed trait MoveCommand extends Command

  sealed trait TurnCommand extends Command

  case object F extends MoveCommand

  case object B extends MoveCommand

  case object L extends TurnCommand

  case object R extends TurnCommand

  case object Unknown extends Command

  case class Rover(position: Position, commands: List[Command], moveResult: MoveResult = Init)

  case class Obstacle(coordinates: Coordinates)

  case class Planet(dimenstion: Dimension, obstables: List[Obstacle])

  sealed trait MoveResult {
    def whenMoved(f: Position => Position): MoveResult =
      this match {
        case Move(p) => Move(f(p))
        case m => m
      }
  }

  case object Init extends MoveResult

  case class Move(position: Position) extends MoveResult

  case class HitObstacle(obstacle: Obstacle) extends MoveResult

  case class GameState(planet: Planet, rover: Rover)

  def init(): IO[GameState] =
    puts("Welcome to the Mars Rover Kata!")
      .flatMap(
        _ => (
          buildPlanet(),
          buildRover()
        ).mapN(GameState)
      )

  def buildPlanet(): IO[Planet] =
    (
      ask("What is the size of the planet?").flatMap(s => tryToIO(parseDimensions(s))),
      ask("Where are the obstacles?").flatMap(s => tryToIO(parseObsacle(s)))
    ).mapN(Planet)

  def buildRover(): IO[Rover] =
    (
      ask("What is the position of the rover?").flatMap(s => tryToIO(parsePosition(s))),
      ask("Waiting commands...").map(parseCommands),
      IO.pure(Init)
    ).mapN(Rover)

  def tryToIO[A](t: Try[A]): IO[A] =
    t fold(
      exception => IO.raiseError(exception),
      value => IO(value)
    )

  def parseDimensions(str: String): Try[Dimension] = Try {
    str.split("x") match {
      case Array(w, h) => Dimension(w.toInt, h.toInt)
    }
  }

  def parseCoordinate(str: String): Try[Coordinates] = Try {
    str.split(",") match {
      case Array(x, y) => Coordinates(x.toInt, y.toInt)
    }
  }

  def parseObsacle(str: String): Try[List[Obstacle]] =
    str.split("#")
      .toList
      .traverse(parseCoordinate(_).map(Obstacle))

  def parsePosition(str: String): Try[Position] =
    parseCoordinate(str)
      .map(coordinates => Position(coordinates, N))

  def parseCommands(str: String): List[Command] =
    str.toList
      .map {
        case 'f' => F
        case 'b' => B
        case 'l' => L
        case 'r' => R
        case _ => Unknown
      }

  def executeCommands(rover: Rover, commands: List[Command]): Rover =
    commands match {
      case Nil => rover
      case cmd :: tail => {

      }
    }

  def execute(game: GameState): GameState =
    game.rover.commands
      .foldLeft(game.rover)(
        (rover: Rover, cmd: Command) => {
          commandExecutor(game.planet, cmd)(rover.position) match {
            case HitObstacle(obst) =>
          }
        }
      )

  game.rover.commands
    .foldLeft(game)(
      (g: GameState, cmd: Command) => {
        val moveResult = commandExecutor(g.planet, cmd)(g.rover.position)


        g.rover.moveResult match {
          case HitObstacle(_) => g
          case _ => g.copy(
            rover = g.rover.copy(
              moveResult = moveResult,
              position =
            )
          )
        }
      }
    )

  def commandExecutor(planet: Planet, command: Command): Position => MoveResult =
    applyCmd(command)
      .andThen(positionNormalizer(planet))
      .andThen(p => obstacleFinder(planet.obstables)(p) match {
        case Some(obstacle) => HitObstacle(obstacle)
        case None => Move(p)
      })

  def applyCmd(command: Command): Position => Position =
    position => command match {
      case c: MoveCommand => move(position, c)
      case c: TurnCommand => turn(position, c)
      case Unknown => position
    }

  def positionNormalizer(planet: Planet): Position => Position =
    position => position.copy(coordinates = Coordinates(
      (position.coordinates.x + planet.dimenstion.w) % planet.dimenstion.w,
      (position.coordinates.y + planet.dimenstion.h) % planet.dimenstion.h
    ))

  def move(position: Position, cmd: MoveCommand): Position = {
    def moveStep(position: Position)(step: Int): Position =
      position.direction match {
        case N => Position(Coordinates(position.coordinates.x, position.coordinates.y + step), N)
        case E => Position(Coordinates(position.coordinates.x + step, position.coordinates.y), E)
        case S => Position(Coordinates(position.coordinates.x, position.coordinates.y - step), S)
        case W => Position(Coordinates(position.coordinates.x - step, position.coordinates.y), W)
      }

    cmd match {
      case F => moveStep(position)(1)
      case B => moveStep(position)(-1)
    }
  }

  def turn(position: Position, cmd: TurnCommand): Position =
    cmd match {
      case L => left(position)
      case R => right(position)
    }

  def left(position: Position): Position =
    position.direction match {
      case N => position.copy(direction = W)
      case W => position.copy(direction = S)
      case S => position.copy(direction = E)
      case E => position.copy(direction = N)
    }

  def right(position: Position): Position = position.direction match {
    case N => position.copy(direction = E)
    case E => position.copy(direction = S)
    case S => position.copy(direction = W)
    case W => position.copy(direction = N)
  }

  def obstacleFinder(obstacles: List[Obstacle]): Position => Option[Obstacle] =
    position => obstacles.find(_ == Obstacle(position.coordinates))

  def display(game: GameState): IO[Unit] =
    IO(render(game.rover))
      .flatMap(puts)

  def render(rover: Rover): String =
    rover.moveResult match {
      case HitObstacle(obst) => s"Ostacolo colpito in posizione $obst, sequenza abortita"
      case _ => s"Rover in posizione ${rover.position.coordinates}, direzione ${rover.position.direction}"
    }

  def displayError(t: Throwable): IO[Unit] =
    puts(s"Porcamadoro: ${t.getMessage}")

  def displayResult(): IO[Unit] =
    puts("Finito")

  def runIO(): IO[Unit] =
    init()
      .map(execute)
      .flatMap(display)
      .attempt
      .flatMap(either => either.fold(displayError, _ => displayResult()))

  def run(): Unit = {
    runIO()
      .unsafeRunSync()
  }
}
