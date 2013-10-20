package synth

import util.expr.{Fraction, Num, Expr}

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 10/11/13
 * Time: 12:23 PM
 * To change this template use File | Settings | File Templates.
 */

case class SeriesEvenTemp(fundamental: Float) extends Series {

  def apply(degree: Int) = SeriesEvenTemp.Interval(degree, fundamental)

}


object SeriesEvenTemp {

  case class Interval(degree: Int, fundamental: Float, override val octave: Int = 0)
    extends Series.Interval {

    override def generatingExpression: Expr = Num(2).pow(Fraction(Num(degree), Num(12)))

    override def octaveUp: Interval = Interval(degree, fundamental, octave + 1)

    override def octaveDown: Series.Interval = Interval(degree, fundamental, octave - 1)
  }

}
