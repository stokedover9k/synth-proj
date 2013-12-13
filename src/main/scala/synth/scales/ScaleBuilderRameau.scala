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

case class ScaleBuilderRameau(fundamentalHz: Float, notes: Seq[String] = ScaleBuilderRameau.allNames)
  extends ScaleBuilder {

  override lazy val build: TypedScale =
    OctaveWrappedScale(allIntervals, ScaleBuilderRameau.allTypes, notes :+ notes(0))

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

  def fifthUpBuilder: ScaleBuilderRameau = {
    val ns = (notes drop 4) ++ (notes take 3) :+ ScaleBuilderRameau.sharps(notes(4))
    ScaleBuilderRameau(build(IntervalType.Fifth).hz, ns)
  }

  def fourthUpBuilder: ScaleBuilderRameau = {
    val ns = ((notes drop 3 take 3) :+ ScaleBuilderRameau.flats(notes(3))) ++ (notes take 3)
    ScaleBuilderRameau(build(IntervalType.Fourth).hz, ns)
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

  lazy val allNames = "C D E F G A B".split("\\s+")

  import IntervalType._

  lazy val allTypes = List(First, Major2, Major3, Fourth, Fifth, Major6, Major7, First)

  lazy val sharps = Map(
    "C" -> "B",
    "G" -> "F#",
    "D" -> "C#",
    "A" -> "G#",
    "E" -> "D#",
    "B" -> "A#"
  )

  lazy val flats = Map(
    "C" -> "F",
    "F" -> "Bb",
    "Bb" -> "Eb",
    "Eb" -> "Ab",
    "Ab" -> "Db",
    "Db" -> "Gb"
  )

  def main(args: Array[String]): Unit = {

    def printScale(scale: TypedScale, builtFrom: Expr): Unit = {
      val pattern = "%-2s %8s | "
      (scale.allNames zip scale.intervals) foreach {
        case (note: String, interval: Series.Interval) =>
          print(pattern.format(note, interval.hzFactor.toString))
      }
      println()
      (scale.allNames zip scale.intervals) foreach {
        case (note: String, interval: Series.Interval) =>
          print(pattern.format("", interval.hzFactor.mult(builtFrom).toString))
      }
      println("\n")
    }

    val builder = ScaleBuilderRameau(528)
    val baseScale = builder.build

    printScale(baseScale, baseScale(0).hzFactor)

    def loopFifths(builder: ScaleBuilderRameau, builtFrom: Expr, num: Int): Unit = if (num > 0) {
      val newBuilder = builder.fifthUpBuilder
      val scale = newBuilder.build
      printScale(scale, builtFrom)
      loopFifths(newBuilder, builtFrom.mult(scale(IntervalType.Fifth).hzFactor), num - 1)
    }

    def loopFourths(builder: ScaleBuilderRameau, builtFrom: Expr, num: Int): Unit = if (num > 0) {
      val newBuilder = builder.fourthUpBuilder
      val scale = newBuilder.build
      printScale(scale, builtFrom)
      loopFifths(newBuilder, builtFrom.mult(scale(IntervalType.Fourth).hzFactor), num - 1)
    }

    loopFifths(builder, baseScale(0).hzFactor, 5)

    loopFourths(builder, baseScale(0).hzFactor, 5)

  }
}
