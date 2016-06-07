package dog

import ai.x.diff._

object HoundTest extends Dog {

  sealed trait Parent
  case class Bar( s: String, i: Int ) extends Parent
  case class Foo( bar: Bar, b: List[Int], parent: Option[Parent] ) extends Parent

  val `dump case class` : TestCase[Unit] = {
    val expected = Assert.fail(s"""Foo( \n  b = Seq( ..., ${red("123")} ),\n  bar = Bar( ..., i = ${red("5")} -> ${green("66")} ),\n  parent = Some( Bar( ..., s = ${red("\"asdf\"")} -> ${green("\"qwer\"")} ) )\n)""")
    val before: Foo = Foo(Bar("asdf", 5), List(123, 1234), Some(Bar("asdf", 5)))
    val after: Foo = Foo(Bar("asdf", 66), List(1234), Some(Bar("qwer", 5)))
    val actual = Hound.equal(before, after)
    Assert.equal(expected, actual)
  }
}
