package synth.scales

import synth.SeriesPythagorean
import util.expr.Fraction

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 10/31/13
 * Time: 12:17 AM
 * To change this template use File | Settings | File Templates.
 */

case class ScaleBuilderPtolemyChromatic(fundamental: Float)
  extends ScaleBuilder {

  private lazy val dodecophonic = ScaleBuilderDodecophonicFull(fundamental).build()

  def allIntervals = {
    val comma = dodecophonic.allTypes.indexOf(IntervalType.Dim5)

    def without2Dim5s[T](seq: Seq[T]) = seq splitAt comma + 1 match {
      case (h, t) => h ++ t.tail
    }

    val intervals = without2Dim5s((dodecophonic.allTypes zip dodecophonic.intervals)) map {
      // adjust diminished 5th
      case (IntervalType.Dim5, i) =>
        new SeriesPythagorean.Interval(i.degree, i.fundamental) {
          override def hzFactor = Fraction(64, 45)
        }
      // for all others...
      case (t, i) => {
        // adjust the intervals which need to be adjusted
        if (ScaleBuilderPtolemyChromatic.adjust8081.contains(t)) {
          val adjustBy = if (i.degree > 0) Fraction(80, 81) else Fraction(81, 80)

          new SeriesPythagorean.Interval(i.degree, i.fundamental) {
            override def hzFactor = super.hzFactor.mult(adjustBy)
          }
        }
        // return the intervals which don't need to be adjusted
        else i
      }
    }

    intervals
  }

  def build(): TypedScale = UniqueScale(allIntervals, ScaleBuilderPtolemyChromatic.allTypes, ScaleBuilderPtolemyChromatic.allNotes)
}

object ScaleBuilderPtolemyChromatic {

  import IntervalType._

  lazy val adjust8081 = Set(Minor2, Minor3, Major3, Minor6, Major6, Major7)

  lazy val allTypes = Seq(First, Minor2, Major2, Minor3, Major3, Fourth, Dim5, Fifth, Minor6, Major6, Minor7, Major7)

  lazy val allNotes = "C|C#|D|D#|E|F|F#|G|G#|A|A#|B".split("\\|").toSeq
}
