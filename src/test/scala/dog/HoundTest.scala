package dog

import scalaz.Equal
import scalaz.std.string._
import scalaz.std.anyVal._
import scalaz.std.list._
import scalaz.std.option._
import ai.x.diff._

object HoundTest extends Dog {

  sealed trait Parent
  case class Bar(s: String, i: Int) extends Parent
  case class Foo(bar: Bar, b: List[Int], parent: Option[Parent]) extends Parent

  val `equal: dump case class` : TestCase[AssertionResult[Unit]] = {
    val expected = Assert.fail(s"""Foo( \n  b = Seq( ..., ${red("123")} ),\n  bar = Bar( ..., i = ${red("5")} -> ${green("66")} ),\n  parent = Some( Bar( ..., s = ${red("\"asdf\"")} -> ${green("\"qwer\"")} ) )\n)""")
    val before: Foo = Foo(Bar("asdf", 5), List(123, 1234), Some(Bar("asdf", 5)))
    val after: Foo = Foo(Bar("asdf", 66), List(1234), Some(Bar("qwer", 5)))
    val actual = Hound.equal(before, after)
    for {
      _ <- Assert.equal(expected, actual)
    } yield expected
  }

  implicit val parentEqual: Equal[Parent] = new Equal[Parent] {
    def equal(a: Parent, b: Parent) = (a, b) match {
      case (a1@Bar(_, _), b1@Bar(_, _)) => barEqual.equal(a1, b1)
      case (a1@Foo(_, _, _), b1@Foo(_, _, _)) => fooEqual.equal(a1, b1)
      case _ => false
    }
  }

  implicit val barEqual: Equal[Bar] = new Equal[Bar] {
    def equal(a: Bar, b: Bar) = Equal[String].equal(a.s, b.s) && Equal[Int].equal(a.i, b.i)
  }

  implicit val fooEqual: Equal[Foo] = new Equal[Foo] {
    def equal(a: Foo, b: Foo) =
      barEqual.equal(a.bar, b.bar) && listEqual[Int].equal(a.b, b.b) && optionEqual[Parent].equal(a.parent, b.parent)
  }

  val `eq: dump case class` : TestCase[Unit] = {
    val before: Foo = Foo(Bar("asdf", 5), List(123, 1234), Some(Bar("asdf", 5)))
    val after: Foo = Foo(Bar("asdf", 66), List(1234), Some(Bar("qwer", 5)))
    val actual = Hound.eq(before, after)
    for {
      expected <- `equal: dump case class`
      _ <- Assert.equal(expected, actual)
    } yield ()
  }
}
