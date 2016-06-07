package dog

import ai.x.diff._
import scalaz.Equal

object Hound {

  def equal[A : DiffShow](expected: A, actual: A): AssertionResult[Unit] =
    if(expected == actual) Assert.pass(())
    else Assert.fail(DiffShow.diff[A](expected, actual).string)

  def eq[A : DiffShow](expected: A, actual: A)(implicit E: Equal[A]): AssertionResult[Unit] =
    if(E.equal(expected, actual)) Assert.pass(())
    else Assert.fail(DiffShow.diff[A](expected, actual).string)
}
