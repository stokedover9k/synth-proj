package synth

import util.expr.{Fraction, Expr}

case class HarmonicSeries(fundamental: Float) extends NoteSeries {

  def apply(degree: Int) = HarmonicSeries.Interval(degree, fundamental)

}

object HarmonicSeries {

  case class Interval(degree: Int, fundamental: Float, override val octave: Int = 0)
    extends NoteSeries.Interval {

    override def generatingExpression: Expr = Fraction(degree + 1, 1)

    override def octaveUp: Interval = Interval(degree, fundamental, octave + 1)

    override def octaveDown: NoteSeries.Interval = Interval(degree, fundamental, octave - 1)
  }

  trait Extracts7Notes extends Series2Scale7[HarmonicSeries] {
    override def sorted7(s: HarmonicSeries): IndexedSeq[Interval] =
      (Seq(1, 2, 4, 6, 8, 10, 12) map (s(_))).toArray[Interval].sortBy(_.hz)
  }

}
