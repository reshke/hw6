package fintech.homework06

import scala.annotation.tailrec

/*
Реализовать тайп класс Eq[A] и синтаксис '===', деривацию для Map Seq Option
Опционально - разработать === для комплексных чисел с возможностью указать точность
*/

trait Eq[A] {
  def equiv(lft: A, rgt: A) : Boolean
}


object Eq {

  class Equiv[A] extends Eq[A] {
    override def equiv(lft: A, rgt: A): Boolean = lft.equals(rgt)
  }

  implicit class MapEq[A, B](left: Map[A, B])(implicit eqKey: (A, A) => Boolean, eqVal: (B, B) => Boolean)
    extends Eq[Map[A, B]] {

//    @tailrec
    private def equiv_times(lft: Map[A, B], rgt: Map[A, B]): Boolean = {
      if (lft.isEmpty && rgt.isEmpty)
        return true

      for ((key, value) <- lft) {
        for ((anotherKey, anotherValue) <- rgt) {
          if (eqKey(key, anotherKey)) {
            if (!eqVal(value, anotherValue)) {
              return false
            }
          }
        }
      }
      true
    }

    override def equiv(lft: Map[A, B], rgt: Map[A, B]): Boolean = {
      equiv_times(lft, rgt)
    }

    def ===(right: Map[A, B]): Boolean = equiv(left, right)
  }

  implicit class SeqEq[A](left: Seq[A])(implicit eq: (A, A) => Boolean) extends Eq[Seq[A]] {

    @tailrec
    private def equiv_times(left: Seq[A], right: Seq[A]): Boolean = {
      if (left.size == right.size) {
        if (left.isEmpty)
          true
        else {
          if (eq(left.head, right.head))
            equiv_times(left.tail, right.tail)
          else
            false
        }
      }
      else
        false
    }

    override def equiv(lft: Seq[A], rgt: Seq[A]): Boolean = {
      equiv_times(lft, rgt)
    }

    def ===(right: Seq[A]): Boolean = equiv(left, right)
  }

  implicit class OptionEq[A](left: Option[A]) (implicit eq: (A, A) => Boolean) extends Eq[Option[A]] {
    override def equiv(lft: Option[A], rgt: Option[A]): Boolean = {
      lft match {
        case Some(value) => rgt match {
          case Some(anotherValue) => eq(value, anotherValue)
          case _ => false
        }
        case None => rgt match {
          case None => true
          case _ => false
        }
      }
    }

    def ===(right: Option[A]): Boolean = equiv(left, right)
  }

  implicit class ComplexNumberEq(left: ComplexNumber)(implicit eq: (ComplexNumber, ComplexNumber) => Boolean)
    extends Eq[ComplexNumber]{

    override def equiv(lft: ComplexNumber, rgt: ComplexNumber): Boolean = eq(lft, rgt)

    def ===(right: ComplexNumber):Boolean = equiv(left, right)
  }

}