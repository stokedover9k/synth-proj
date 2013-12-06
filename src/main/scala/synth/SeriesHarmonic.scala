package synth

import util.expr.{Fraction, Expr}
import scala.collection.mutable

case class SeriesHarmonic(fundamental: Float) extends Series {

  def apply(degree: Int) = SeriesHarmonic.Interval(degree, fundamental)

}

object SeriesHarmonic {

  case class Interval(degree: Int, fundamental: Float, override val octave: Int = 0)
    extends Series.Interval {

    override def generatingExpression: Expr = Fraction(degree + 1, 1)

    override def octaveUp: Interval = Interval(degree, fundamental, octave + 1)

    override def octaveDown: Series.Interval = Interval(degree, fundamental, octave - 1)
  }

  def sorted13(s: SeriesHarmonic): IndexedSeq[Interval] = {
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
    (addVals(new mutable.HashMap[Float, Interval], 0, 12).values.toIndexedSeq ++ Seq(s(0).octaveUp)).sortBy(_.hz)
  }
}
