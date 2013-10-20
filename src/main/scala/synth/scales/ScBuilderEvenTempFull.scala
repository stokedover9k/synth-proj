package synth.scales

import synth.EvenTempSeries

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 10/20/13
 * Time: 2:44 PM
 * To change this template use File | Settings | File Templates.
 */

case class ScBuilderEvenTempFull(fundamental: Float)
  extends ScBuilder {

  lazy val series = EvenTempSeries(fundamental)

  def allIntervals = (0 to 11 map (series(_))) :+ series(12).octaveUp

  def build(): TypedSc = OctaveWrappedSc(allIntervals, ScBuilderEvenTempFull.allTypes, ScBuilderEvenTempFull.allNotes)
}


object ScBuilderEvenTempFull {

  lazy val allTypes = IntervalType.getAll :+ IntervalType.First

  lazy val allNotes = "C|C#|D|D#|E|F|F#|G|G#|A|A#|B|C".split("\\|").toSeq
}
