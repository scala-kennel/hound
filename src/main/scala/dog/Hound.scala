package dog

import ai.x.diff._

object Hound {

  def equal[A : DiffShow](expected: A, actual: A): AssertionResult[Unit] =
    if(expected == actual) Assert.pass(())
    else Assert.fail(DiffShow.diff[A](expected, actual).string)
}
