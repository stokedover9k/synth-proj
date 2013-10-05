package synth

import util.expr.{Fraction, Expr}

case class HarmonicSeries(fundamental: Float) extends NoteSeries {

  def apply(degree: Int) = HarmonicSeries.Interval(degree, fundamental)

}

object HarmonicSeries {
  case class Interval(degree: Int, fundamental: Float) extends NoteSeries.Interval {
    override def generatingExpression: Expr = Fraction(degree + 1, 1)
    override def inNextOctave: Interval = Interval(degree, fundamental * 2)
  }

  trait Extracts7Notes extends Series2Scale7[HarmonicSeries] {
    override def extract7(s: HarmonicSeries): Iterable[Interval] = 0 until 7 map (s(_))
  }
}
