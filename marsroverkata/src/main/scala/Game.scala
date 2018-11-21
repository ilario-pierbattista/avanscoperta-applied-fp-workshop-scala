package marsroverkata

import cats.effect._
import cats.data._
import cats.implicits._

import scala.io.StdIn._
import scala.util.{Failure, Success, Try}

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

  case object F extends Command

  case object B extends Command

  case object L extends Command

  case object R extends Command

  case class Rover(position: Position, commands: List[Command])

  case class Obstacle(coordinates: Coordinates)

  case class Planet(dimenstion: Dimension, obstables: List[Obstacle])

  case class GameSnapshot(planet: Planet, rover: Rover)

  def init(): IO[GameSnapshot] =
    puts("Welcome to the Mars Rover Kata!")
      .flatMap(_ => buildPlanet())
      .flatMap(
        planet =>
          buildRover(planet)
            .map(rover => GameSnapshot(planet, rover))
      )

  def buildPlanet(): IO[Planet] =
    (
      ask("What is the size of the planet?").map(parseDimensions),
      ask("Where are the obstacles?").map(parseObsacle)
    ).mapN(Planet)

  def buildRover(planet: Planet): IO[Rover] =
    (
      ask("What is the position of the rover?").map(parsePosition(planet)),
      ask("Waiting commands...").map(parseCommands)
    ).mapN(Rover)

  def parseDimensions(str: String): Try[Dimension] =
    try {
      return str match {
        case Array(w, h) => Success(Dimension(w.toInt, h.toInt))
        case _ => throw
      }
    } catch (e: Exception) {
      return Failure(e)
    }

  def parseObsacle(str: String): List[Obstacle] = ???

  def parsePosition(planet: Planet)(str: String): Position = ???

  def parseCommands(str: String): List[Command] = ???

  def execute(gameSnapshot: GameSnapshot): Rover = ???

  def display(rover: Rover): IO[Unit] =
    IO(render(rover))
      .flatMap(puts)

  def render(rover: Rover): String = ???

  def runIO(): IO[Unit] =
    init()
      .map(execute)
      .flatMap(display)

  def run(): Unit = {
    runIO().unsafeRunSync()

    //println("Welcome to the Mars Rover Kata!")
    //println("What is the size of the planet?")
    //println("Where are the obstacles?")
    //println("What is the position of the rover?")
    //println("Waiting commands...")
  }
}
