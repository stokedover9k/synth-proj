package util.expr

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 9/27/13
 * Time: 1:59 AM
 * To change this template use File | Settings | File Templates.
 */
abstract class NumericExpr extends Expr {
  override def sign = if( toFloat < 0 ) MINUS else PLUS
  def abs: NumericExpr
}

object Num {
  def apply(n: Int):     WholeNum = WholeNum(n)
  def apply(n: Float): DecimalNum = DecimalNum(n)
}

case class WholeNum (n: Int) extends NumericExpr {
  def toFloat = n
  def toInt = n
  override def opposite: Expr = new WholeNum(-n)
  def abs: NumericExpr = WholeNum(if( n < 0 ) -n else n)

  override def toString = n.toString
}

case class DecimalNum (n: Float) extends NumericExpr {
  def toFloat = n
  override def opposite: Expr = DecimalNum(-n)
  def abs: NumericExpr = DecimalNum(if( n < 0 ) - n else n)

  override def toString = n.toString
}