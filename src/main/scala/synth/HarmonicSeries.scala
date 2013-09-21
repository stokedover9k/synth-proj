package synth

import util.Fraction



case class HarmonicSeries(fundamental: Float) extends NoteSeries {

  case class Interval(degree: Int, fundamental: Float) extends NoteSeries.Interval {

    def hzSimpleRatio = Fraction(degree, 1)
  }

  def apply(degree: Int) = Interval(degree, fundamental)
}
