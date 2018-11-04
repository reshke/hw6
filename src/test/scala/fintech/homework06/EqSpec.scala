package fintech.homework06
import org.scalatest.{FlatSpec, Matchers}
import fintech.homework06.Eq.OptionEq, fintech.homework06.Eq.SeqEq,
fintech.homework06.Eq.MapEq, fintech.homework06.Eq.ComplexNumberEq

class EqSpec extends FlatSpec with Matchers {

  it should "work well with Option" in {
    Some(1) === Some(2) should be(false)
    Some(1) === Some(1) should be(true)
  }

  it should "work well with None" in {
    Some(1) === None should be(false)
    None === Some(22) should be(false)
  }


  it should "work well with option implicit eq" in {
    implicit def equiv(left : Int, right :Int) : Boolean =  left % 5 == right % 5

    Some(1) === Some(6) should be(true)
    Some(1) === Some(11) should be(true)

    Some(2) === Some(3) should be(false)
  }


  it should "work well with Option with any types" in {

    Some(1.0) === Some(1.0) should be(true)
    Some(1.0) === Some(1) should be(true)

    Some("any string here") === Some("some another string here") should be(false)
    Some("aaaa") === Some("aaaa") should be(true)

    Some(12 -> Seq("a")) === Some(12 -> Seq("a")) should be(true)
    Some(13 -> Seq("a")) === Some(12 -> Seq("a")) should be(false)
    Some(12 -> Seq("a")) === Some(12 -> Seq("ab")) should be(false)
  }

  it should "work well with Seq" in {
    val seq: Seq[Int] = Seq(1, 2, 3, 4, 5)
    val anotherSeq: Seq[Int] = Seq(1, 2, 3, 4, 5)

    seq === anotherSeq should be(true)
  }

  it should "work well with seq implicit eq" in {
    implicit def equiv(lft: Int, rgt: Int): Boolean = lft == rgt + 1

    val seq: Seq[Int] = Seq(2, 3, 4, 5, 6)
    val anotherSeq: Seq[Int] = Seq(1, 2, 3, 4, 5)
    val notEquivSeq : Seq[Int] = Seq(2,4,4,5,6)

    //    SeqEq(seq)(equiv).===(anotherSeq) should be(true)
    seq === anotherSeq should be(true)
    seq === notEquivSeq should be(false)
  }

  it should "work well with more implicit eq" in {
    implicit def equiv(lft: String, rgt: String): Boolean =
      lft.length > 0 && rgt.length > 0 && lft.charAt(0) == rgt.charAt(0)

    val seq: Seq[String] = Seq("audhwef", "berifoi", "jfif", "ijeif", "fkjorf")
    val anotherSeq: Seq[String] =  Seq("aerfer", "bgergoi", "jgergrege", "ierg", "fergf")
    val notEquivSeq: Seq[String] = Seq("audhwef", "frifoi", "wefwefif", "weff", "wefwef")

    seq === anotherSeq should be(true)
    notEquivSeq === seq should be(false)
  }

  it should "work well with map" in {
    val map: Map[Int, Int] = Map(1 -> 2, 2 -> 3, 3 -> 4)
    val anotherMap : Map[Int, Int] = Map(1 -> 2, 2 -> 3, 3 -> 4)
    val notEquivMap : Map[Int, Int] = Map(1 -> 2, 2 -> 3, 3 -> 5)

    map === anotherMap should be (true)
    map === notEquivMap should be(false)
    anotherMap === notEquivMap should be(false)
  }


  it should "work well with map with implicit key eq" in {
    implicit def keyEquiv(left  :Int, right : Int):Boolean = left % 5 == right % 5

    val map: Map[Int, Int] = Map(1 -> 2, 2 -> 3, 3 -> 4)
    val anotherMap : Map[Int, Int] = Map(1 -> 2, 7 -> 3, 8 -> 4)
    val notEquivMap : Map[Int, Int] = Map(1 -> 2, 2 -> 3, 3 -> 5)

    map === anotherMap should be (true)
    map === notEquivMap should be(false)
    anotherMap === notEquivMap should be(false)
  }

  it should "work well with map with implicit key and value eq" in {
    implicit def keyEquiv(left  :Int, right : Int):Boolean = left % 5 == right % 5
    implicit def valueEquiv(left  :String, right : String):Boolean =
      left.length > 0 && right.length > 0 && left.charAt(0) == right.charAt(0)

    val map: Map[Int, String] = Map(1 -> "anfjerf", 2 -> "bifjeoir", 3 -> "cerhfio")
    val anotherMap : Map[Int, String] = Map(1 -> "awiejf", 7 -> "bwepof", 8 -> "cwoef")
    val notEquivMap : Map[Int, String] = Map(1 -> "edfwef", 2 -> "berer", 8 -> "fewf")

    map === anotherMap should be(true)
    map === notEquivMap should be(false)
    anotherMap === notEquivMap should be(false)
  }

  def accuracyComparator(left : ComplexNumber, right: ComplexNumber)(accuracy : Double):Boolean =
    (right - left).absoluteValue() < accuracy

  it should "work well with Complex Number, big accuracy case " in {
    implicit def bigAccuracyComparator(left : ComplexNumber, right : ComplexNumber) : Boolean =
      accuracyComparator(left, right)(1e-9)

    val value = ComplexNumber(20, 22)
    val anotherValue = ComplexNumber(20, 22)
    val nonEquivValue = ComplexNumber(19, 23)

    value === anotherValue should be(true)
    value === nonEquivValue should be(false)
    anotherValue === nonEquivValue should be(false)
  }

  it should "work well with Complex Number, little accuracy case " in {
    implicit def littleAccuracyComparator(left : ComplexNumber, right : ComplexNumber) : Boolean =
      accuracyComparator(left, right)(10)

    val value = ComplexNumber(20, 22)
    val anotherValue = ComplexNumber(21, 24)
    val nonEquivValue = ComplexNumber(19, 23)

    value === anotherValue should be(true)
    value === nonEquivValue should be(true)
    anotherValue === nonEquivValue should be(true)
  }

}
