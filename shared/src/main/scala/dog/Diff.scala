package dog

import shapeless._
import scalaz.{IList, Show, Equal}
import scalaz.std.anyVal._
import scalaz.std.string._

sealed abstract class DiffTree extends Product with Serializable

object DiffTree {

  final case class Identical(value: String) extends DiffTree
  final case class Different(left: String, right: String) extends DiffTree
  final case class ValueWithName(name: String, value: DiffTree) extends DiffTree
  final case class Node(children: IList[DiffTree]) extends DiffTree
  final case class Error(e: Throwable) extends DiffTree

  def identical[F](value: F)(implicit F: Show[F]): DiffTree = Identical(F.shows(value))

  def different[E, F](l: E, r: F)(implicit E: Show[E], F: Show[F]): DiffTree = Different(E.shows(l), F.shows(r))

  def node(children: IList[DiffTree]): DiffTree = Node(children)

  def valueWithName(name: String, value: DiffTree): DiffTree = ValueWithName(name, value)
}

trait Diff[F] {
  def diff(left: F, right: F): DiffTree
}

object Diff {

  def apply[F](implicit F: Diff[F]): Diff[F] = F

  private[this] def primitive[F: Equal: Show] = new Diff[F] {
    override def diff(left: F, right: F) =
      if (Equal[F].equal(left, right)) DiffTree.identical(left)
      else DiffTree.different(left, right)
  }

  implicit val booleanDiff: Diff[Boolean] = primitive
  implicit val byteDiff: Diff[Byte] = primitive
  implicit val shortDiff: Diff[Short] = primitive
  implicit val floatDiff: Diff[Float] = primitive
  implicit val doubleDiff: Diff[Double] = primitive
  implicit val intDiff: Diff[Int] = primitive
  implicit val longDiff: Diff[Long] = primitive
  implicit val stringDiff: Diff[String] = primitive
}

object DiffAuto extends LabelledTypeClassCompanion[Diff] {
  override val typeClass: LabelledTypeClass[Diff] =
    new DiffTypeClassImpl()
}

private final class DiffTypeClassImpl() extends LabelledTypeClass[Diff] {

  private[this] def dump[R <: Coproduct](v: R): String = v match {
    case Inl(l) => l.toString
    case Inr(_) => v.toString
  }

  override def coproduct[L, R <: Coproduct](name: String, CL: => Diff[L], CR: => Diff[R]) = {
    lazy val cl = CL
    lazy val cr = CR
    new Diff[L :+: R] {
      override def diff(left: L :+: R, right: L :+: R) = (left, right) match {
        case (Inl(l1), Inl(l2)) => cl.diff(l1, l2)
        case (Inr(r1), Inr(r2)) => cr.diff(r1, r2)
        case (Inl(l), Inr(r)) => DiffTree.different(name, dump(r))
        case (Inr(l), Inl(r)) => DiffTree.different(name, r.toString)
      }
    }
  }

  override val emptyCoproduct = new Diff[CNil] {
    override def diff(left: CNil, right: CNil) =
      DiffTree.Error(new Exception("Methods in CNil type class instance should never be called.?"))
  }

  override val emptyProduct = new Diff[HNil] {
    override def diff(left: HNil, right: HNil) = DiffTree.node(IList.empty)
  }

  override def product[H, T <: HList](name: String, H: Diff[H], T: Diff[T]) = new Diff[H :: T] {
    override def diff(left: H :: T, right: H :: T) = {
      val head = DiffTree.valueWithName(name, H.diff(left.head, right.head))
      DiffTree.node(T.diff(left.tail, right.tail) match {
        case DiffTree.Node(l) if l.isEmpty => IList.single(head)
        case tail => IList(head, tail)
      })
    }
  }

  override def project[F, G](instance: => Diff[G], to: F => G, from: G => F) = new Diff[F] {
    override def diff(left: F, right: F) =
      instance.diff(to(left), to(right))
  }
}
