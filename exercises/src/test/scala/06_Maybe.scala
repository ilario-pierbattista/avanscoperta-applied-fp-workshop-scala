package exercises

import minitest._

/*
 * Functions can't always return a value.
 * In this scenario they are called: partial functions.
 * We can convert them into total functions
 * with the introduction of effects.
 *
 *  f:  InType => Effect[OutType]
 */

object MaybeTests extends SimpleTestSuite {

  /*
   * TODO: remove all nulls
   */

  case class Qty(value: Int)

  sealed trait Opt[+A]
  case class Som[A](a: A) extends Opt[A]
  case object Non extends Opt[Nothing]

  def toQty(value: String): Opt[Qty] =
    if (value.matches("^[0-9]+$")) Som(Qty(value.toInt))
    else Non

  test("valid qty") {
    assertEquals(toQty("100"), Som(Qty(100)))
  }

  test("invalid qty") {
    assertEquals(toQty("asd"), Non)
    assertEquals(toQty("1 0 0"), Non)
    assertEquals(toQty(""), Non)
    assertEquals(toQty("-10"), Non)
  }
}
