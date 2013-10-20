package synth.scales

import synth.SeriesPythagorean
import scala.collection.immutable

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 10/20/13
 * Time: 2:07 PM
 * To change this template use File | Settings | File Templates.
 */

case class ScaleBuilderPythagHepto(fundamental: Float)
  extends ScaleBuilder {

  lazy val series = SeriesPythagorean(fundamental)

  def allIntervals = ((-1 to 5 map (series(_)) sortBy (_.hz)) :+ series(0).octaveUp).toIndexedSeq

  def build(): TypedScale with WesternModes =
    new OctaveWrappedScale(allIntervals, ScaleBuilderPythagHepto.allTypes, ScaleBuilderPythagHepto.allNotes) with WesternModes {
      def Ionian: ScaleMode = mode(0)
      def Dorian: ScaleMode = mode(1)
      def Phrygian: ScaleMode = mode(2)
      def Lydian: ScaleMode = mode(3)
      def Mixolydian: ScaleMode = mode(4)
      def Aeolian: ScaleMode = mode(5)
      def Locrian: ScaleMode = mode(6)
      def IonianOctave: ScaleMode = mode(7)
    }
}


object ScaleBuilderPythagHepto {

  import IntervalType._

  lazy val allTypes = immutable.IndexedSeq(First, Major2, Major3, Fourth, Fifth, Major6, Major7, First)

  lazy val allNotes = "C|D|E|F|G|A|B|C".split("\\|").toIndexedSeq
}
