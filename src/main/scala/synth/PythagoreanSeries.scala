package synth

import util.expr._
import util.expr.Div
import util.expr.WholeNum

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 9/25/13
 * Time: 11:46 PM
 * To change this template use File | Settings | File Templates.
 */

case class PythagoreanSeries(fundamental: Float) extends NoteSeries {

  def apply(degree: Int) = Interval(degree, fundamental)

  case class Interval(degree: Int, fundamental: Float) extends NoteSeries.Interval {
    def generatingExpression: Expr = Div(Num(3), Num(2)).pow(WholeNum(degree))
  }
}




