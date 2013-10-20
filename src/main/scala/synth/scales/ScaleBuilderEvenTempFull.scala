package synth.scales

import synth.SeriesEvenTemp

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 10/20/13
 * Time: 2:44 PM
 * To change this template use File | Settings | File Templates.
 */

case class ScaleBuilderEvenTempFull(fundamental: Float)
  extends ScaleBuilder {

  lazy val series = SeriesEvenTemp(fundamental)

  def allIntervals = (0 to 11 map (series(_))) :+ series(12).octaveUp

  def build(): TypedScale = OctaveWrappedScale(allIntervals, ScaleBuilderEvenTempFull.allTypes, ScaleBuilderEvenTempFull.allNotes)
}


object ScaleBuilderEvenTempFull {

  lazy val allTypes = IntervalType.getAll :+ IntervalType.First

  lazy val allNotes = "C|C#|D|D#|E|F|F#|G|G#|A|A#|B|C".split("\\|").toSeq
}
