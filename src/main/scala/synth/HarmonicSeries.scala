package synth

import util.expr.{Fraction, Expr}

case class HarmonicSeries(fundamental: Float) extends NoteSeries {

  def apply(degree: Int) = Interval(degree, fundamental)

  case class Interval(degree: Int, fundamental: Float) extends NoteSeries.Interval {
    def generatingExpression: Expr = Fraction(degree, 1)
  }
}

object HarmonicSeries {

  trait Extracts7Notes extends Series2Scale7[HarmonicSeries] {
    override def extract7(s: HarmonicSeries): Iterable[Float] = 0 until 7 map (s(_).hz)
  }
}
