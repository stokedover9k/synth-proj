package synth.scales

import synth.PythagoreanSeries
import scala.collection.immutable

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 10/20/13
 * Time: 2:07 PM
 * To change this template use File | Settings | File Templates.
 */

case class ScBuilderPythagHepto(fundamental: Float)
  extends ScBuilder {

  lazy val series = PythagoreanSeries(fundamental)

  def allIntervals = ((-1 to 5 map (series(_)) sortBy (_.hz)) :+ series(0).octaveUp).toIndexedSeq

  def build(): TypedSc with WesternModes =
    new OctaveWrappedSc(allIntervals, ScBuilderPythagHepto.allTypes, ScBuilderPythagHepto.allNotes) with WesternModes {
      def Ionian: ScMode = mode(0)
      def Dorian: ScMode = mode(1)
      def Phrygian: ScMode = mode(2)
      def Lydian: ScMode = mode(3)
      def Mixolydian: ScMode = mode(4)
      def Aeolian: ScMode = mode(5)
      def Locrian: ScMode = mode(6)
      def IonianOctave: ScMode = mode(7)
    }
}


object ScBuilderPythagHepto {

  import IntervalType._

  lazy val allTypes = immutable.IndexedSeq(First, Major2, Major3, Fourth, Fifth, Major6, Major7, First)

  lazy val allNotes = "C|D|E|F|G|A|B|C".split("\\|").toIndexedSeq
}
