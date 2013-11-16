package synth

import util.expr.{Num, Fraction, Expr}

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 11/16/13
 * Time: 6:25 PM
 * To change this template use File | Settings | File Templates.
 */

class SeriesPythagMeanTone(override val fundamental: Float)
  extends Series {

  override def apply(degree: Int) = SeriesPythagMeanTone.Interval(degree, fundamental, 0)
}


object SeriesPythagMeanTone {

  case class Interval(degree: Int, fundamental: Float, override val octave: Int = 0)
    extends Series.Interval {

    override def generatingExpression: Expr = Fraction(3, 2).mult(Fraction(80, 81).pow(Fraction(1, 4))).pow(Num(degree))

    override def octaveUp: Interval = Interval(degree, fundamental, octave + 1)

    override def octaveDown: Series.Interval = Interval(degree, fundamental, octave - 1)
  }

}
