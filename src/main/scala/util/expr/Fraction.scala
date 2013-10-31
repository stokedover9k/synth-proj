package util.expr

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 9/26/13
 * Time: 6:50 PM
 * To change this template use File | Settings | File Templates.
 */

class Fraction(private val num: Int, private val denom: Int) extends BinOp with LeftAssoc {

  def leftOp: Expr = WholeNum(num)

  def rightOp: Expr = WholeNum(denom)

  def opString: String = "/"

  override def toString: String = if(denom == 1) num.toString else super.toString

  override def withParens: String = if(denom == 1) num.toString else super.withParens

  def toFloat: Float = num.toFloat / denom

  override def opposite: Fraction = new Fraction(-num, denom)

  def mult(o: Int): Fraction = Fraction(num * o, denom)

  def div(o: Int): Fraction = Fraction(num, o * denom)

  def plus(o: Int): Fraction = Fraction(num + o * denom, denom)

  def minus(o: Int): Fraction = plus(-o)

  def pow(o: Int): Fraction =
    if(o > 0)      new Fraction(Math.pow(num, o).toInt, Math.pow(denom, o).toInt)
    else if(o < 0) new Fraction(denom, num).pow(-o)
    else           new Fraction(1, 1)

  override def mult(o: Expr): Expr = o match {
    case WholeNum(n: Int) => mult(n)
    case f: Fraction => Fraction(num * f.num, denom * f.denom)
    case _ => super.mult(o)
  }

  override def div(o: Expr): Expr = o match {
    case WholeNum(n: Int) => div(n)
    case f: Fraction => Fraction(num * f.denom, denom * f.num)
    case _ => super.div(o)
  }

  override def plus(o: Expr): Expr = o match {
    case WholeNum(n: Int) => plus(n)
    case _ => super.plus(o)
  }

  override def minus(o: Expr): Expr = o match {
    case WholeNum(n: Int) => minus(n)
    case _ => super.minus(o)
  }

  override def pow(o: Expr): Expr = o match {
    case WholeNum(n: Int) => pow(n)
    case _ => super.pow(o)
  }
}



object Fraction {

  def apply(num: Int, denom: Int): Fraction = {
    if( denom < 0 ) apply(-num, -denom)
    else {
      val d = util.gcd(num.abs, denom)
      new Fraction(num / d, denom / d)
    }
  }

  def apply(num: WholeNum, denom: WholeNum): Fraction = apply(num.toInt, denom.toInt)
}
