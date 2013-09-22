package util2

import util.gcd

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 9/22/13
 * Time: 1:37 PM
 * To change this template use File | Settings | File Templates.
 */

abstract class Sign { def opposite: Sign }
object PLUS  extends Sign { def opposite = MINUS }
object MINUS extends Sign { def opposite = PLUS  }

abstract class Expr {
  def toFloat: Float
  def sign: Sign = PLUS
  def opposite: Expr = UMinus(this)
}

object Expr {
  implicit def Expr2Float(e: Expr): Float = e.toFloat
}



abstract class NumericExpr extends Expr {
  override def sign = if( toFloat < 0 ) MINUS else PLUS
  def abs: NumericExpr
}

case class WholeNum (n: Int) extends NumericExpr {
  def toFloat = n
  def toInt = n
  override def opposite: Expr = new WholeNum(-n)
  def abs: NumericExpr = WholeNum(if( n < 0 ) -n else n)
}

case class DecimalNum (n: Float) extends NumericExpr {
  def toFloat = n
  override def opposite: Expr = DecimalNum(-n)
  def abs: NumericExpr = DecimalNum(if( n < 0 ) - n else n)
}

object Num {
  def apply(n: Int):     WholeNum = WholeNum(n)
  def apply(n: Float): DecimalNum = DecimalNum(n)
}



abstract class UniOp extends Expr
abstract class BinOp extends Expr



case class UMinus(e: Expr) extends UniOp {
  def toFloat: Float = -e
  override def opposite: Expr = e
}

case class Div(a: Expr, b: Expr) extends BinOp {
  def toFloat: Float = a / b
  override def opposite = Div(a.opposite, b)
}

case class Mult(a: Expr, b: Expr) extends BinOp {
  def toFloat: Float = a * b
  override def opposite = Mult(a.opposite, b)
}



class Fraction (val num: Expr, val denom: Expr) extends Expr {
  def toFloat: Float = num / denom

  private def reduce: Fraction = {
    (num, denom) match {
      case (a: WholeNum, b: WholeNum) => {
        val d = gcd(a.n.abs, b.n.abs)
        new Fraction(WholeNum(a.toInt / d), WholeNum(b.toInt / d))
      }
      case _ => this
    }
  }
}

object Fraction {
  def apply(num: Int, denom: Int): Fraction = Fraction(WholeNum(num), WholeNum(denom))
  def apply(num: Float, denom: Float): Fraction = Fraction(DecimalNum(num), DecimalNum(denom))

  def apply(num: Expr, denom: Expr): Fraction = {
    if( denom.sign == MINUS )
      new Fraction(num.opposite, denom.opposite).reduce
    else
      new Fraction(num, denom).reduce
  }
}
