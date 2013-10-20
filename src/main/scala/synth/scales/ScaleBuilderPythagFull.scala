package synth.scales

import synth.{SeriesEvenTemp, SeriesPythagorean}

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 10/20/13
 * Time: 2:06 PM
 * To change this template use File | Settings | File Templates.
 */

case class ScaleBuilderPythagFull(fundamental: Float)
  extends ScaleBuilder {

  lazy val series = SeriesPythagorean(fundamental)

  def allIntervals = (-1 until 12 map (series(_)) sortBy (_.hz)) :+ series(12).octaveUp

  def build(): TypedScale = UniqueScale(allIntervals, ScaleBuilderPythagFull.allTypes, ScaleBuilderPythagFull.allNotes)
}


object ScaleBuilderPythagFull {

  import IntervalType._

  lazy val allTypes = Seq(First, Minor2, Major2, Minor3, Major3, Fourth, Dim5, Dim5, Fifth, Minor6, Major6, Minor7, Major7, First)

  lazy val allNotes = "C|C#|D|D#|E|F|F#|Gb|G|G#|A|A#|B|C".split("\\|").toSeq
}
