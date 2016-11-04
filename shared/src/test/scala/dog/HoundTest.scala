package dog

import scalaz._
import scalaz.Kleisli._
import scalaz.std.string._
import scalaz.std.anyVal._
import scalaz.IList._
import scalaz.Maybe._
import dog.Diff._
import dog.DiffAuto._

sealed abstract class Parent extends Product with Serializable
final case class Bar(s: String, i: Int) extends Parent
final case class Foo(bar: Bar, b: IList[Int], parent: Maybe[Parent]) extends Parent

object HoundTest extends Dog with Assert {

  val hound: Hound = new Hound {}

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
      barEqual.equal(a.bar, b.bar) && Equal[IList[Int]].equal(a.b, b.b) && Equal[Maybe[Parent]].equal(a.parent, b.parent)
  }

  def run[A](test: TestCaseAp[A]): ValidationResult[A] =
    test.foldMap(testCaseApRunner)(kleisliApplicative).run(Param.id)

  def expectedFailed(message: String): ValidationResult[Unit] =
    run(fail(message))

  val `equal: dump case class` = TestCase {
    val expected = expectedFailed(s""".bar.i
  expected: 5
  actual:   66
.b.head
  expected: 123
  actual:   1234
.b.tail
  expected: "ICons"
  actual:   "[]"
.parent.a.s
  expected: "asdf"
  actual:   "qwer"""")
    val before: Parent = Foo(Bar("asdf", 5), IList(123, 1234), Maybe.just(Bar("asdf", 5)))
    val after: Parent = Foo(Bar("asdf", 66), IList.single(1234), Maybe.just(Bar("qwer", 5)))
    val actual = run(hound.equal(before, after))
    for {
      _ <- equal(expected, actual).lift
    } yield ((expected, before, after))
  }

  val `eq: dump case class` = TestCase {
    for {
      values <- `equal: dump case class`
      _ <- equal(values._1, run(hound.eq(values._2, values._3))).lift
    } yield ()
  }
}
