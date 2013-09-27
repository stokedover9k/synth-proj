package util.expr

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 9/27/13
 * Time: 2:04 AM
 * To change this template use File | Settings | File Templates.
 */
abstract class UniOp extends Expr { def operand: Expr }



case class UMinus(operand: Expr) extends UniOp {
  def toFloat: Float = -operand
  override def opposite: Expr = operand
}
