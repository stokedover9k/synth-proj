package synth.scales

import synth.{EvenTempSeries, PythagoreanSeries}

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 10/20/13
 * Time: 2:06 PM
 * To change this template use File | Settings | File Templates.
 */

case class ScBuilderPythagFull(fundamental: Float)
  extends ScBuilder {

  lazy val series = PythagoreanSeries(fundamental)

  def allIntervals = (-1 until 12 map (series(_)) sortBy (_.hz)) :+ series(12).octaveUp

  def build(): TypedSc = UniqueSc(allIntervals, ScBuilderPythagFull.allTypes, ScBuilderPythagFull.allNotes)
}


object ScBuilderPythagFull {

  import IntervalType._

  lazy val allTypes = Seq(First, Minor2, Major2, Minor3, Major3, Fourth, Dim5, Dim5, Fifth, Minor6, Major6, Minor7, Major7, First)

  lazy val allNotes = "C|C#|D|D#|E|F|F#|Gb|G|G#|A|A#|B|C".split("\\|").toSeq
}
