package synth

import util.expr.{Fraction, Expr}

//package synth
//
//import util.Fraction
//
//
//
//case class HarmonicSeries(fundamental: Float) extends NoteSeries {
//
//  case class Interval(degree: Int, fundamental: Float) extends NoteSeries.Interval {
//
//    def hzSimpleRatio = Fraction(degree, 1)
//  }
//
//  def apply(degree: Int) = Interval(degree, fundamental)
//}

case class HarmonicSeries(fundamental: Float) extends NoteSeries {

  def apply(degree: Int) = Interval(degree, fundamental)

  case class Interval(degree: Int, fundamental: Float) extends NoteSeries.Interval {
    def generatingExpression: Expr = Fraction(degree, 1)
  }
}


