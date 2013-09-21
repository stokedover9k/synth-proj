package util

/**
 * Created with IntelliJ IDEA.
 * User: yuriy
 * Date: 9/16/13
 * Time: 12:11 PM
 * To change this template use File | Settings | File Templates.
 */

/*
 * This Fraction supports integer numerator and denominator.
 * The fractions are not automatically reduced at construction or mathematical operations.
 */
case class Fraction(num: Int, denom: Int) {
  import util.gcd

  def reduce: Fraction = { val d = gcd(num, denom); new Fraction(num/d, denom/d) }

  def * (factor: Int) = new Fraction(num * factor, denom)
  def * (factor: Fraction) = new Fraction(num * factor.num, denom * factor.denom)

  def / (factor: Int) = new Fraction(num, denom * factor)
  def / (factor: Fraction) = new Fraction(num * factor.denom, denom * factor.num)

  def + (term: Int): Fraction = this.+(Fraction(term))
  def + (term: Fraction) = new Fraction(num * term.denom + term.num * denom, denom * term.denom)

  def - (term: Int): Fraction = this.-(Fraction(term))
  def - (term: Fraction) = new Fraction(num * term.denom - term.num * denom, num * denom)

  def toFloat = num.toFloat / denom
  def toDouble = num.toDouble / denom
  override def toString = num + "/" + denom
  def toRatioString     = num + ":" + denom
}



object Fraction {

  def apply(num: Int) = new Fraction(num, 1)
}

