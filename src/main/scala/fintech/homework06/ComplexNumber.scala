package fintech.homework06
import scala.math.sqrt

case class ComplexNumber(realPart : Double , imaginaryPart : Double ) {
  val eps : Double = 1e-9

  def absoluteValue(): Double = sqrt(realPart * realPart + imaginaryPart * imaginaryPart)

  def +(complexNumber: ComplexNumber): ComplexNumber =
    ComplexNumber(realPart + complexNumber.realPart, imaginaryPart + complexNumber.imaginaryPart)

  def *(coefficient: Double): ComplexNumber = ComplexNumber(realPart * coefficient, imaginaryPart * coefficient)

  def *(factor: ComplexNumber): ComplexNumber =
    ComplexNumber(realPart * factor.realPart - imaginaryPart * factor.imaginaryPart,
      realPart * factor.imaginaryPart + imaginaryPart * factor.realPart)

  def -(complexNumber: ComplexNumber): ComplexNumber = this + complexNumber * -1

  override def hashCode(): Int = realPart.hashCode() ^ imaginaryPart.hashCode()

  override def equals(obj: Any): Boolean = obj match {
    case ComplexNumber(a, b) => math.abs(realPart - a) < eps && math.abs(imaginaryPart - b) < eps
    case _ => false
  }
}