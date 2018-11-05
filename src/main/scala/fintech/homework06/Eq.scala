package fintech.homework06

import scala.annotation.tailrec

/*
Реализовать тайп класс Eq[A] и синтаксис '===', деривацию для Map Seq Option
Опционально - разработать === для комплексных чисел с возможностью указать точность
*/

trait Eq[A] {
  def equiv(lft: A, rgt: A) : Boolean
}

object EqSyntax {
  implicit class EqOps[T: Eq](val self: T) {
    def equiv(other: T): Boolean = implicitly[Eq[T]].equiv(self, other)

    def ====(other : T) : Boolean = self.equiv(other)
  }
}

trait EqInstances {

  import EqSyntax._

  // instances для примитивных типов
  implicit val stringEq = new Eq[String] {
    override def equiv(lft: String, rgt: String): Boolean = lft == rgt
  }

  implicit val intEq = new Eq[Int] {
    override def equiv(lft: Int, rgt: Int): Boolean = lft == rgt
  }

  implicit val doubleEq = new Eq[Double] {
    override def equiv(lft: Double, rgt: Double): Boolean = math.abs(rgt - lft) < 1e-9
  }

  implicit def mapEq[K: Eq, V: Eq]: Eq[Map[K, V]] = new Eq[Map[K, V]] {
    override def equiv(lft: Map[K, V], rgt: Map[K, V]): Boolean = {
      if (lft.isEmpty && rgt.isEmpty)
        true
      else {
        for ((key, value) <- lft) {
          for ((anotherKey, anotherValue) <- rgt) {
            if (key ==== anotherKey) {
              return if (value ==== anotherValue) equiv(lft - key, rgt - anotherKey) else false
            }
          }
        }
        false
      }
    }
  }

  implicit def OptionEq[A: Eq]: Eq[Option[A]] = new Eq[Option[A]] {
    override def equiv(lft: Option[A], rgt: Option[A]): Boolean = {
      (lft, rgt) match {
        case (Some(value), Some(anotherValue)) => value ==== anotherValue
        case (Some(_), None) => false
        case (None, Some(_)) => false
        case (None, None) => true
      }
    }
  }

  implicit def SeqEq[A: Eq]: Eq[Seq[A]] = new Eq[Seq[A]] {

    @tailrec
    private def equiv_times(left: Seq[A], right: Seq[A]): Boolean = {
      if (left.isEmpty && right.isEmpty)
        true
      else {
        if (left.isEmpty || right.isEmpty)
          false
        else {
          if (left.head ==== right.head) equiv_times(left.tail, right.tail) else false
        }
      }
    }

    override def equiv(lft: Seq[A], rgt: Seq[A]): Boolean = {
      equiv_times(lft, rgt)
    }
  }

  implicit def ComplexNumberEq(implicit accuracy: Double = 1e-9): Eq[ComplexNumber] =
    (lft, rgt) => (lft - rgt).absoluteValue() < accuracy

}

object Eq extends EqInstances