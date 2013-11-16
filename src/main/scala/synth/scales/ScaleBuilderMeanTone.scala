package synth.scales

import synth.SeriesPythagMeanTone

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 11/16/13
 * Time: 5:22 PM
 * To change this template use File | Settings | File Templates.
 */

case class ScaleBuilderMeanTone(fundamental: Float)
  extends ScaleBuilder {

  lazy val series = new SeriesPythagMeanTone(fundamental)

  def allIntervals = (-6 to 10 map (series(_)) sortBy (_.hz))

  def build(): TypedScale = UniqueScale(allIntervals, ScaleBuilderMeanTone.allTypes, ScaleBuilderMeanTone.allNotes)
}


object ScaleBuilderMeanTone {

  import IntervalType._

  lazy val allTypes = Seq(First, Minor2, Minor2, Major2, Minor3, Minor3, Major3, Fourth, Dim5, Dim5, Fifth, Minor6, Minor6, Major6, Minor7, Minor7, Major7)

  lazy val allNotes = "C|C#|Db|D|D#|Eb|E|F|F#|Gb|G|G#|Ab|A|A#|Bb|B".split("\\|").toSeq
}