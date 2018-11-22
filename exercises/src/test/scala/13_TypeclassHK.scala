package exercises

import minitest._

/*
 * In a type system we deal with different kind of types.
 *
 * Type like Int, String, Date, Person, etc. are called
 * 'proper type'. In polymorphic function it is represented
 * by symbols like A, B, C, etc.
 * def sum[A](x: A, y: A): A = ???
 * These types have kind: *
 *
 * Instead types like List[A], Option[A], Box[A], etc.
 * are called 'type constructor', because they are open
 * until associated with a proper type like List[Int],
 * Option[String], Box[Person], etc. In the polymorphic
 * function definition nothing changes.
 * def concat[A](x: List[A], y: List[A]): List[A] = ???
 * These types have kind: * -> *
 * aka accepts a type and returns a type.
 *
 * In Scala and few others languages we can express
 * a function polymorphic over the type constructor.
 * def concat[F[_], A](x: F[A], y: F[A]): F[A] = ???
 * In these cases we talk about Higher-Order Kinds.
 * These types have kind: * -> * -> *
 * aka accepts a type constructor and returns
 * a type constructor.
 *
 * It's similar to the idea of High-Order Functions
 * aka function that accepts/returns other functions.
 */

object TypeclassHKTests extends SimpleTestSuite {

  /*
   * TODO: Define push and pop operations.
   *       Implements class instance.
   *       Implements MRUList.add.
   */

  trait Functor[F[_]] {
    def map[A, B](fa:F[A])(f: A => B): F[B]
  }

  trait Stack[F[_]] {
    def push[A](container: F[A], item: A): F[A]
  }

  implicit val listStack: Stack[List] = new Stack[List] {
    override def push[A](container: List[A], item: A): List[A] = item :: container
  }

  object Stack {
    def apply[F[_]](implicit x: Stack[F]): Stack[F] = x
  }

  object MRUList {
    def add[F[_] : Stack, A](value: A, items: F[A])(implicit s: Stack[F]): F[A] =
      s.push(items, value)
  }

  test("add an element to the MRU list") {
    import MRUList._

    // ignore("implement missing functions")
    val items = List("first")
    val result = add("second", items)
    assertEquals(result, List("second", "first"))
  }
}
