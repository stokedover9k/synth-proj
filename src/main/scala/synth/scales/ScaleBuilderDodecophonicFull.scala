package synth.scales

import synth.SeriesPythagorean

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 10/20/13
 * Time: 9:56 PM
 * To change this template use File | Settings | File Templates.
 */

case class ScaleBuilderDodecophonicFull(fundamental: Float)
  extends ScaleBuilder {

  lazy val series = SeriesPythagorean(fundamental)

  def allIntervals = -6 to 6 map (series(_)) sortBy (_.hz)

  def build(): TypedScale = UniqueScale(allIntervals, ScaleBuilderDodecophonicFull.allTypes, ScaleBuilderDodecophonicFull.allNotes)
}


object ScaleBuilderDodecophonicFull {

  import IntervalType._

  lazy val allTypes = Seq(First, Minor2, Major2, Minor3, Major3, Fourth, Dim5, Dim5, Fifth, Minor6, Major6, Minor7, Major7)

  lazy val allNotes = "C|Db|D|Eb|E|F|Gb|F#|G|Ab|A|Bb|B".split("\\|").toSeq
}
