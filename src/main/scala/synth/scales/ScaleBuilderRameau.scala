package synth.scales

import util.expr.{Fraction, Num, Expr}
import synth.Series
import synth.Series.Interval

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 11/26/13
 * Time: 3:11 PM
 * To change this template use File | Settings | File Templates.
 */

case class ScaleBuilderRameau(fundamentalHz: Float) extends ScaleBuilder {
  def build(): TypedScale = OctaveWrappedScale(allIntervals, ScaleBuilderRameau.allTypes, ScaleBuilderRameau.allNames)

  private def makeInterval(oct: Int)(e: (Expr, Int)): Series.Interval = e match {
    case (ratio: Expr, deg: Int) =>
      new Series.Interval {
        override val octave: Int = oct

        val degree: Int = deg

        def octaveUp: Interval = makeInterval(octave + 1)(ratio.mult(Num(2)), deg)

        def octaveDown: Interval = makeInterval(octave - 1)(ratio.div(Num(2)), deg)

        val fundamental: Float = fundamentalHz
        val generatingExpression: Expr = ratio
      }
  }

  lazy val allIntervals = {
    def is = ScaleBuilderRameau.ratios.zipWithIndex map makeInterval(0)
    val iss = is.groupBy(_.hz).map(_._2.head).toIndexedSeq.sortBy(_.hz)
    iss :+ iss(0).octaveUp
  }
}

object ScaleBuilderRameau {

  protected lazy val ratios = {
    val i1 = Fraction(1, 1) // first
    val i3 = Fraction(5, 4) // third
    val i4 = Fraction(4, 3) // fourth
    val i5 = Fraction(3, 2) // fifth

    def triad(from: Fraction) = List(from, from.mult(i3), from.mult(i5))

    (triad(i1) ++ triad(i4) ++ triad(i5))
  }

  lazy val allNames = "C D E F G A B C".split("\\s+")

  import IntervalType._

  lazy val allTypes = List(First, Major2, Major3, Fourth, Fifth, Major6, Major7, First)
}
