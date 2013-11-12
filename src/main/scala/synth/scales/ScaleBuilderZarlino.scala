package synth.scales

import util.expr.{Expr, Fraction}

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 11/12/13
 * Time: 1:40 PM
 * To change this template use File | Settings | File Templates.
 */

case class ScaleBuilderZarlino(fundamental: Float)
  extends ScaleBuilder {

  import ScaleBuilderZarlino.CustomInterval

  def getIntervals: Seq[CustomInterval] = {
    def construct(degree: Int, from: CustomInterval, ratio: Expr, octave: Integer = null): CustomInterval =
      new CustomInterval(degree, from.generatingExpression.mult(ratio), from.fundamental, if (octave == null) from.octave else octave)

    val five = Fraction(5, 4)
    val six = Fraction(6, 5)
    val fiveDown = Fraction(4, 5)
    val sixDown = Fraction(5, 6)

    val i1 = new CustomInterval(1, Fraction(1, 1), fundamental)
    val i3 = construct(3, i1, five)
    val i5 = construct(5, i3, six)
    val i7 = construct(7, i5, five)
    val i2 = construct(2, i7, six)

    val i8 = construct(8, i1, Fraction(1, 1), octave = 1)
    val i6 = construct(6, i8, sixDown, octave = 0)
    val i4 = construct(4, i6, fiveDown, octave = 0)

    Seq(i1, i2, i3, i4, i5, i6, i7, i8)
  }

  def build(): TypedScale = OctaveWrappedScale(getIntervals, ScaleBuilderZarlino.allTypes, ScaleBuilderZarlino.allNotes)
}


object ScaleBuilderZarlino {

  class CustomInterval(override val degree: Int,
                       override val generatingExpression: Expr,
                       override val fundamental: Float,
                       override val octave: Int = 0)
    extends synth.Series.Interval {

    override def octaveUp: CustomInterval =
      new CustomInterval(degree, generatingExpression, fundamental, octave + 1)

    override def octaveDown: CustomInterval =
      new CustomInterval(degree, generatingExpression, fundamental, octave - 1)
  }

  lazy val allTypes = IntervalType.getWholeSteps :+ IntervalType.First

  lazy val allNotes = "C|D|E|F|G|A|B|C".split("\\|").toSeq
}
