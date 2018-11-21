package exercises

import minitest._

/*
 * In OOP model object that incapsulate data and expose behaviours.
 * This two concepts are brigs togheter thanks to class definitions.
 *
 * In FP data and behaviours are modelled with two different tools:
 * - Algebraic Data Type (ADT) to model data
 * - Function to model behaviours
 *
 * An ADT is an immutable data structure that compose other types.
 * There are two common kinds of composition strategy:
 * - Product type: put many types togheter. e.g. struct in C, POJO in JAVA.
 *                 It's called product because all the possible values of a Tuple[String, Int] is
 *                 the *product* of all possible String with all possible Int.
 *                 Useful to model indipendent data in AND e.g. a Person is composed by a name *and* an age.
 *
 * - Sum type:     model exclusive types e.g. union in C, enum in JAVA.
 *                 Sum types correspond to disjoint unions of sets.
 *                 It's called sum because all the possible values of a Either[String, Int] is
 *                 the *sum* of all possible String with all possible Int.
 *                 Useful to model dipendant data in OR e.g. the Light is on *or* off.
 *
 * We can mix ADT as we want, like a product type that compose a type with a sum type.
 */

object ModelData extends SimpleTestSuite {

  // Typical product type in Scala
  case class Person(name: String, age: Int)

  // Typical sum type in Scala
  sealed trait LightState

  case class On() extends LightState

  case class Off() extends LightState

  /*
   * TODO: Model Scopa the italian card game, below the game description. :-)
   *       After modeling the domain implements the test following the description of ignores.
   *
   * DESCIPTION:
   *       It is played (let simplify) between two players with
   *       a standard Italian 40-card deck, divided into four suits: Cups, Golds, Swords, Clubs.
   *       The values on the cards range numerically from one through seven,
   *       plus three face cards in each suit: Knight (worth 8), Queent (worth 9) and King (worth 10).
   *       Each player receives three cards. The dealer will also place four cards face up on the table.
   *
   * ADD YOUR CODE HERE INSIDE THE OBJECT
   */

  sealed trait Suit

  case object Cups extends Suit

  case object Golds extends Suit

  case object Swords extends Suit

  case object Clubs extends Suit

  type values = Int

  sealed trait CardValue

  case class NumberValue(amount: values) extends CardValue

  case object Knight extends CardValue

  case object Queen extends CardValue

  case object King extends CardValue

  case class Card(suit: Suit, value: CardValue)

  case class Hand(cards: List[Card])

  case class Player(hand: Hand)

  case class Deck(cards: List[Card])

  case class Table(cards: List[Card])

  val player1 = Player(
    Hand(List(
      Card(Golds, NumberValue(2)),
      Card(Swords, NumberValue(5)),
      Card(Clubs, NumberValue(7))
    ))
  )

  val player2 = Player(
    Hand(List(
      Card(Cups, NumberValue(1)),
      Card(Cups, NumberValue(2)),
      Card(Golds, Queen),
    ))
  )

  test("represent initial match state") {
    ignore("build the first player w/ 2 of Golds, 5 of Swords and 7 of Clubs")
    ignore("build the second player w/ 1 of Cups, 2 of Clubs and 9 of Golds")
    ignore("build the table w/ 4 of Clubs, 10 of Swords, 8 of Golds and 1 of Swords")
    ignore("build the deck w/ only 1 of Swords and 10 of Clubs")
    ignore("build the game")
    () // don't delete

  }
}
