package synth

import util.expr.{Fraction, Expr}

case class HarmonicSeries(fundamental: Float) extends NoteSeries {

  def apply(degree: Int) = Interval(degree, fundamental)

  case class Interval(degree: Int, fundamental: Float) extends NoteSeries.Interval {
    def generatingExpression: Expr = Fraction(degree, 1)
  }
}


