package synth.scales

import scala.collection.immutable
import synth.Series.Interval

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 11/12/13
 * Time: 6:47 PM
 * To change this template use File | Settings | File Templates.
 */

abstract class ScaleBuilderWesternHepto {

  def allIntervals: Seq[synth.Series.Interval]

  def allTypes: Seq[IntervalType]

  def allNames: Seq[String]

  def build(): TypedScale with WesternModes = {
    val sc = OctaveWrappedScale(allIntervals, allTypes, allNames)

    new OctaveWrappedScale(sc.intervals, sc.allTypes, sc.allNames) with WesternModes {
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
}

object ScaleBuilderWesternHepto {

  def apply(buildFrom: TypedScale, types: Seq[IntervalType]) = {

    val indices: Seq[Int] = types map (buildFrom.allTypes.indexOf(_))

    assert(indices.forall(_ != -1))

    val last = indices.size - 1

    var intervals = indices map buildFrom.intervals
    if( intervals(0).hz == intervals(last).hz )
      intervals = intervals.updated(last, intervals(0).octaveUp)

    println(intervals map (_.hz))

    new ScaleBuilderWesternHepto {
      override val allIntervals: Seq[Interval] = intervals
      override val allTypes: Seq[IntervalType] = indices map buildFrom.allTypes
      override val allNames: Seq[String] = indices map buildFrom.allNames
    }
  }
}
