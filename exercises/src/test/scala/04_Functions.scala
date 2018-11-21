package exercises

import minitest._

/*
 * ADT models data while Function models behaviour.
 * A function is simply something that accepts an input value
 * and produces an output value.
 * In more accademic terms it connects a Domain to a Codomain.
 * Functions are described/documented by it's type definition.
 *
 *  f:  InType => OutType
 */

object FunctionsTests extends SimpleTestSuite {

  /*
   * TODO: implements functions maked with `???`
   */

  val asString: Double => String = in => in.toString

  val parseString: String => Int = in => in.toInt

  val reciprocal: Int => Double = in => 1.0 / in

  val reciprocalString: String => String = x => {
    asString(
      reciprocal(
        parseString(x)
      )
    )
  }

  val reciprocalStringComposed: String => String = asString compose reciprocal compose parseString

  //def |>[A, B, C](implicit g: B => C, f: A => B): A => C = { x => g(f(x))}

  //val reciprocalStringAndthen: String => String = parseString |> reciprocal |> toString

  test("from string to string throught reciprocal") {
    // ignore("use existing function to compute a reciprocal in string")
    assertEquals(reciprocalString("42"), "0.023809523809523808")
    assertEquals(reciprocalStringComposed("42"), "0.023809523809523808")
    //assertEquals(reciprocalStringAndthen("42"), "0.023809523809523808")
  }
}
