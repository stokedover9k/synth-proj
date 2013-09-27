package util

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 9/26/13
 * Time: 6:42 PM
 * To change this template use File | Settings | File Templates.
 */
package object expr {

  def priority(arg: Expr): Int = {
    arg match {
      case _: NumericExpr => Integer.MAX_VALUE
      case _: UMinus => 11
      case _: Pow  => 10
      case _: Mult => 5
      case _: Div  => 5
      case _: Fraction => 5
      case _: Add => 4
      case _: Dif => 4
      case _ => Integer.MIN_VALUE
    }
  }

}
