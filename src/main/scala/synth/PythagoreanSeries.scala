package synth

import util.expr.{Num, Expr, Div, WholeNum}

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 9/25/13
 * Time: 11:46 PM
 * To change this template use File | Settings | File Templates.
 */

case class PythagoreanSeries(fundamental: Float) extends NoteSeries {

  def apply(degree: Int) = PythagoreanSeries.Interval(degree, fundamental)

}

object PythagoreanSeries {
  case class Interval(degree: Int, fundamental: Float) extends NoteSeries.Interval {
    override def generatingExpression: Expr = Div(Num(3), Num(2)).pow(WholeNum(degree))
    override def inNextOctave: Interval = Interval(degree, fundamental * 2)
  }

  trait Extracts7Notes extends Series2Scale7[PythagoreanSeries] {
    override def extract7(s: PythagoreanSeries): Iterable[Interval] = -1 to 5 map (s(_))
  }
}