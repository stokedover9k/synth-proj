package synth

import util.expr._
import util.expr.WholeNum

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 9/25/13
 * Time: 11:46 PM
 * To change this template use File | Settings | File Templates.
 */

case class SeriesPythagorean(fundamental: Float) extends Series {

  def apply(degree: Int) = SeriesPythagorean.Interval(degree, fundamental)

}

object SeriesPythagorean {

  case class Interval(degree: Int, fundamental: Float, override val octave: Int = 0)
    extends Series.Interval {

    override def generatingExpression: Expr = Fraction(Num(3), Num(2)).pow(WholeNum(degree))

    override def octaveUp: Interval = Interval(degree, fundamental, octave + 1)

    override def octaveDown: Series.Interval = Interval(degree, fundamental, octave - 1)
  }
}