package synth

import util.expr.{Fraction, Expr}
import scala.collection.mutable
import synth.oldscales.Series2Scale7

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

  def sorted13(s: HarmonicSeries): IndexedSeq[Interval] = {
    def addVals(m: mutable.HashMap[Float, Interval], index: Int, num: Int): mutable.HashMap[Float, Interval] = {
      val i = s(index)
      if (num <= 0)
        m
      else if (m.contains(i.hz))
        addVals(m, index + 1, num)
      else {
        m.put(i.hz, i)
        addVals(m, index + 1, num - 1)
      }
    }
    addVals(new mutable.HashMap[Float, Interval], 0, 13).values.toIndexedSeq.sortBy(_.hz)
  }
}
