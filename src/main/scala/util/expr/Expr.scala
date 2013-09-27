package util.expr

abstract class Expr {
  def toFloat: Float
  def sign: Sign = PLUS
  def opposite: Expr = UMinus(this)

  def plus(other: Expr): Expr = Add(this, other)
  def minus(other: Expr): Expr = Dif(this, other)
  def mult(other: Expr): Expr = Mult(this, other)
  def div(other: Expr): Expr = Div(this, other)
  def pow(exponent: Expr): Expr = new Pow(this, exponent)

  def priority: Int = util.expr.priority(this)

  def withParens: String = "(%s)".format(toString)
}

object Expr {
  implicit def Expr2Float(e: Expr): Float = e.toFloat
}
