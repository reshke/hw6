package fintech.homework06
import org.scalatest.{FlatSpec, Matchers}
import fintech.homework06.EqSyntax._
import fintech.homework06.Eq._

class EqSpec extends FlatSpec with Matchers {

  it should "work well with Option" in {
    val value : Option[Int] = Some(1)
    val equivValue : Option[Int] = Some(1)
    val nonEquivValue : Option[Int] = Some(2)

    equivValue ==== value should be(true)
    value ==== nonEquivValue should be(false)
    equivValue ==== nonEquivValue should be(false)
  }

  it should "work well with None" in {
    val value : Option[Int] = None
    val equivValue : Option[Int] = None
    val nonEquivValue : Option[Int] = Some(1)

    equivValue ==== value should be(true)
    value ==== nonEquivValue should be(false)
    equivValue ==== nonEquivValue should be(false)
  }


  it should "work well with option implicit eq" in {
    implicit val intEq = new Eq[Int] {
      override def equiv(lft: Int, rgt: Int): Boolean = lft % 5 == rgt % 5
    }
    val value : Option[Int] = Some(6)
    val equivValue : Option[Int] = Some(1)
    val nonEquivValue : Option[Int] = Some(2)

    equivValue ==== value should be(true)
    value ==== nonEquivValue should be(false)
    equivValue ==== nonEquivValue should be(false)
  }

//
//  it should "work well with Option with any types" in {
//
//    Some(1.0) === Some(1.0) should be(true)
//    Some(1.0) === Some(1) should be(true)
//
//    Some("any string here") === Some("some another string here") should be(false)
//    Some("aaaa") === Some("aaaa") should be(true)
//
//    Some(12 -> Seq("a")) === Some(12 -> Seq("a")) should be(true)
//    Some(13 -> Seq("a")) === Some(12 -> Seq("a")) should be(false)
//    Some(12 -> Seq("a")) === Some(12 -> Seq("ab")) should be(false)
//  }

  it should "work well with Seq" in {
    val seq: Seq[Int] = Seq(1, 2, 3, 4, 5)
    val anotherSeq: Seq[Int] = Seq(1, 2, 3, 4, 5)

    seq ==== anotherSeq
  }

  it should "work well with seq with int implicit eq" in {
    implicit val intEq = new Eq[Int] {
      override def equiv(lft: Int, rgt: Int): Boolean = lft == rgt + 1
    }

    val seq: Seq[Int] = Seq(2, 3, 4, 5, 6)
    val anotherSeq: Seq[Int] = Seq(1, 2, 3, 4, 5)
    val notEquivSeq: Seq[Int] = Seq(2, 4, 4, 5, 6)

    seq ==== anotherSeq should be(true)
    seq ==== notEquivSeq should be(false)
    anotherSeq ==== notEquivSeq should be(false)
  }

  it should "work well with seq with string implicit eq" in {
    implicit val stringEq = new Eq[String] {
      override def equiv(lft: String, rgt: String): Boolean =
        lft.length > 0 && rgt.length > 0 && lft.charAt(0) == rgt.charAt(0)
    }

    val seq: Seq[String] = Seq("audhwef", "berifoi", "jfif", "ijeif", "fkjorf")
    val anotherSeq: Seq[String] =  Seq("aerfer", "bgergoi", "jgergrege", "ierg", "fergf")
    val notEquivSeq: Seq[String] = Seq("audhwef", "frifoi", "wefwefif", "weff", "wefwef")

    seq ==== anotherSeq should be(true)
    notEquivSeq ==== seq should be(false)
    anotherSeq ==== notEquivSeq should be(false)
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
    implicit val intEq = new Eq[Int] {
      override def equiv(lft: Int, rgt: Int): Boolean = lft % 5 == rgt % 5
    }

    val map: Map[Int, String] = Map(1 -> "a", 2 -> "b", 3 -> "c")
    val anotherMap : Map[Int, String] = Map(1 -> "a", 7 -> "b", 8 -> "c")
    val notEquivMap : Map[Int, String] = Map(1 -> "a", 2 -> "bww", 3 -> "c")

    map ==== anotherMap should be (true)
    map ==== notEquivMap should be(false)
    anotherMap ==== notEquivMap should be(false)
  }

  it should "work well with map with implicit key and value eq " in {
    implicit val intEq = new Eq[Int] {
      override def equiv(lft: Int, rgt: Int): Boolean = lft % 5 == rgt % 5
    }

    val map: Map[Int, Int] = Map(1 -> 1, 2 -> 2, 3 -> 3)
    val anotherMap : Map[Int, Int] = Map(1 -> 11, 7 -> 22, 8 -> 28)
    val notEquivMap : Map[Int, Int] = Map(1 -> 12, 2 -> 12, 3 -> 12)

    map ==== anotherMap should be (true)
    map ==== notEquivMap should be(false)
    anotherMap ==== notEquivMap should be(false)
  }

  it should "work well with map with different implicit key and value eq" in {
    implicit val intEq = new Eq[Int] {
      override def equiv(lft: Int, rgt: Int): Boolean = lft % 5 == rgt % 5
    }
    implicit val stringEq = new Eq[String] {
      override def equiv(lft: String, rgt: String): Boolean =
        lft.length > 0 && rgt.length > 0 && lft.charAt(0) == rgt.charAt(0)
    }

    val map: Map[Int, String] = Map(1 -> "anfjerf", 2 -> "bifjeoir", 3 -> "cerhfio")
    val anotherMap : Map[Int, String] = Map(1 -> "awiejf", 7 -> "bwepof", 8 -> "cwoef")
    val notEquivMap : Map[Int, String] = Map(1 -> "edfwef", 2 -> "berer", 8 -> "fewf")

    map ==== anotherMap should be(true)
    map ==== notEquivMap should be(false)
    anotherMap ==== notEquivMap should be(false)
  }

  it should "work well with Complex Number, big accuracy case " in {
    val value = ComplexNumber(20, 22)
    val anotherValue = ComplexNumber(20, 22)
    val nonEquivValue = ComplexNumber(19, 23)

    value ==== anotherValue should be(true)
    value ==== nonEquivValue should be(false)
    anotherValue ==== nonEquivValue should be(false)
  }

  it should "work well with Complex Number, little accuracy case " in {
    implicit val accuracy : Double = 11

    val value = ComplexNumber(20, 22)
    val anotherValue = ComplexNumber(21, 24)
    val anotherEquivValue = ComplexNumber(19, 23)

    value ==== anotherValue should be(true)
    value ==== anotherEquivValue should be(true)
    anotherValue ==== anotherEquivValue should be(true)
  }

}
