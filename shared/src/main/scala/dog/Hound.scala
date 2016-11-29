package dog

import scalaz._
import scalaz.syntax.apply._

trait Hound { self =>

  def assert: Hound = self

  private[this] val coreAssert: Assert = new Assert {}

  private[this] def treeToAp[A](tree: DiffTree): TestCaseAp[A] = {
    def go(acc: Throwable \/ String, path: String, tree: DiffTree): Throwable \/ String = tree match {
      case DiffTree.Identical(_) => acc
      case DiffTree.Different(l, r) => acc.map{ v =>
        val result = s"""${path}
  expected: ${l}
  actual:   ${r}"""
        if(v.isEmpty) result
        else s"""${v}
${result}"""
      }
      case DiffTree.Node(children) =>
        children.foldLeft(acc)(go(_, path, _))
      case DiffTree.ValueWithName(name, value) =>
        val cp = if(path == ".") path + name else path + "." + name
        go(acc, cp, value)
      case DiffTree.Error(e) => -\/(e)
    }
    go(\/-(""), ".", tree).fold(
      e => TestCase.handle(IList.single(e)),
      v => fail(v)
    )
  }

  def equal[A](expected: A, actual: A)(implicit D: Diff[A]): TestCaseAp[Unit] =
    if(expected == actual) pass(())
    else treeToAp(D.diff(expected, actual))

  def eq[A](expected: A, actual: A)(implicit E: Equal[A], D: Diff[A]): TestCaseAp[Unit] =
    if(E.equal(expected, actual)) pass(())
    else treeToAp(D.diff(expected, actual))

  def apply(pred: => Boolean): TestCaseAp[Unit] = coreAssert(pred)

  def pred(p: => Boolean): TestCaseAp[Unit] = coreAssert.pred(p)

  def pass[A](value: A): TestCaseAp[A] = coreAssert.pass(value)

  def fail[A](reason: String): TestCaseAp[A] = coreAssert.fail(reason)

  def trap[A](f: => A): TestCase[Throwable] = coreAssert.trap(f)

  implicit class TestCaseApUnitSyntax private[dog](val test: TestCaseAp[Unit]) {

    private[this] def compose(other: TestCaseAp[Unit]) =
      (test |@| other) { case (_, _) => () }

    def equal[A: Diff](expected: A, actual: A): TestCaseAp[Unit] =
      compose(self.equal(expected, actual))

    def eq[A: Equal: Diff](expected: A, actual: A): TestCaseAp[Unit] =
      compose(self.eq(expected, actual))

    def pass[A](value: A): TestCaseAp[A] = test.map(_ => value)

    def fail[A](reason: String): TestCaseAp[A] =
      (test |@| self.fail[A](reason)) { case (_, v) => v }

    def pred(p: Boolean): TestCaseAp[Unit] = compose(self.pred(p))
  }
}
