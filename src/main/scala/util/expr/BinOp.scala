package util.expr

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 9/27/13
 * Time: 2:03 AM
 * To change this template use File | Settings | File Templates.
 */
abstract class BinOp extends Expr { def leftOp: Expr;  def rightOp: Expr;  def opString: String }



trait LeftAssoc extends BinOp {
  override def toString: String = {
    def l = if(leftOp.priority < priority) leftOp.withParens else leftOp.toString
    def r = if(rightOp.priority <= priority) rightOp.withParens else rightOp.toString
    l + opString + r
  }
}

trait RightAssoc extends BinOp {
  override def toString: String = {
    def l = if(leftOp.priority <= priority) leftOp.withParens else leftOp.toString
    def r = if(rightOp.priority < priority) rightOp.withParens else rightOp.toString
    l + opString + r
  }
}



case class Add(leftOp: Expr, rightOp: Expr) extends BinOp with LeftAssoc {
  def toFloat: Float = leftOp.toFloat + rightOp
  override def opString = "+"
}

case class Dif(leftOp: Expr, rightOp: Expr) extends BinOp with LeftAssoc {
  def toFloat: Float = leftOp - rightOp
  override def opString = "-"
}



case class Div(leftOp: Expr, rightOp: Expr) extends BinOp with LeftAssoc {
  def toFloat: Float = leftOp / rightOp
  override def opposite = Div(leftOp.opposite, rightOp)
  override def opString = "/"
}

case class Mult(leftOp: Expr, rightOp: Expr) extends BinOp with LeftAssoc {
  def toFloat: Float = leftOp * rightOp
  override def opposite = Mult(leftOp.opposite, rightOp)
  override def opString = "*"
}



case class Pow(leftOp: Expr, rightOp: Expr) extends BinOp with RightAssoc {
  def toFloat: Float = Math.pow(leftOp.toFloat, rightOp.toFloat).toFloat
  override def opString = "^"
}
