package exercises

import minitest._

/*
 * Pattern match enable the structural recurtion
 * a fancy name to express a way to distch logic
 * by type and data. It goes hand in hand with ADT
 * specially Sum Type. Think, how we can implement
 * some special logic `foo` for an "exclusive-or"
 * data type?
 */

object PatternMatchDispatch extends SimpleTestSuite {

  /*
   * TODO: rewrite the dispatch logic
   *       from polymorphic dispatch (a fundamental OOP technique)
   *       to pattern match dispatch.
   *       Keep tests green.
   */

  sealed trait Direction {
    def turnRight: Direction = this match {
      case N() => E()
      case E() => S()
      case S() => W()
      case W() => N()
    }
    def turnLeft: Direction = this match {
      case N() => W()
      case W() => S()
      case S() => E()
      case E() => N()
    }
  }

  case class N() extends Direction
  case class E() extends Direction
  case class W() extends Direction
  case class S() extends Direction

  test("turn right") {
    assertEquals(N().turnRight, E())
    assertEquals(E().turnRight, S())
    assertEquals(S().turnRight, W())
    assertEquals(W().turnRight, N())
  }

  test("turn left") {
    assertEquals(N().turnLeft, W())
    assertEquals(W().turnLeft, S())
    assertEquals(S().turnLeft, E())
    assertEquals(E().turnLeft, N())
  }
}
