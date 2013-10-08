package synth

import synth.Notes.Name
import synth.NoteSeries.Interval

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 10/7/13
 * Time: 3:29 PM
 * To change this template use File | Settings | File Templates.
 */

abstract class MyScale {
  def size: Int

  def apply(index: Int) = intervalAt(index)

  def intervalAt(index: Int): NoteSeries.Interval

  def noteAt(index: Int): Notes.Name
}



case class NamedInterval(name: Notes.Name, interval: NoteSeries.Interval)



abstract class NamedIntervalsScale extends MyScale {

  protected def getNamed(index: Int): NamedInterval

  override def intervalAt(index: Int): Interval = getNamed(index).interval

  override def noteAt(index: Int): Name = getNamed(index).name
}



abstract class ScaleWithNames
  extends NamedIntervalsScale
  with NamedNotes {

  protected def intervals: IndexedSeq[NamedInterval]

  protected lazy val namedOctave: NamedInterval =
    getNamed(0) match { case NamedInterval(n, i) => NamedInterval(n.octave, i.octaveUp) }

  override protected def getNamed(index: Int): NamedInterval = if( index == size ) namedOctave else intervals(index)

  override def size: Int = intervals.size

  def getInterval(noteName: Name): Interval = {
    intervals.find( _.name == noteName ).map(_.interval).getOrElse(
      throw sys.error("No note %s in scale %s".format(noteName, toString))
    )
  }
}



class ScaleWithModes protected( override protected val intervals: IndexedSeq[NamedInterval] )
  extends ScaleWithNames {

  def modeAt(offset: Int): ScaleWithModes = {
    val (head, tail) = intervals.splitAt(offset)
    new ScaleWithModes(
      tail ++ (head map {
        i => NamedInterval(i.name, i.interval.octaveUp)
      })
    )
  }

}





object ScaleWithModes extends CreatesScaleWithModes[ScaleWithModes] {
  override protected def buildFromNamedIntervals(intervals: IndexedSeq[NamedInterval]): ScaleWithModes =
  new ScaleWithModes(intervals)
}



trait MakesScalesFromNamedIntervals[S <: ScaleWithNames] {
  def buildFromNamedIntervals(intervals: IndexedSeq[NamedInterval]): S
}



trait CreatesScaleWithModes[S <: MakesModes] extends MakesModes[S] {

  def apply(intervals: IndexedSeq[NoteSeries.Interval], names: IndexedSeq[Notes.Name]): S = {
    if (intervals.size != names.size)
      throw sys.error("number of names doesn't equal number of intervals")

    apply( (intervals, names).zipped.map {
      (i, n) => NamedInterval(n, i)
    } )
  }

  //  protected def trustingConstructor(intervals: IndexedSeq[NamedInterval]): S

  protected def apply(intervals: IndexedSeq[NamedInterval]): S = {
    ensureIntervals(intervals map { _.interval } )
    //    trustingConstructor(intervals)
    buildFromNamedIntervals(intervals)
  }

  private def ensureIntervals(intervals: IndexedSeq[NoteSeries.Interval]) =
    if (!(intervals, intervals.tail).zipped.forall
      ((a, b) =>
        a.hz <= b.hz &&
          b.hz < 2 * intervals.head.hz)
    ) throw sys.error("intervals must be ordered by frequency and within an octave")
}



class WesternScale7 protected ( override val intervals: IndexedSeq[NamedInterval] )
  extends ScaleWithModes(intervals)
  with WesternModes[WesternScale7] {

  protected def nthModeOffset(n: Int): Int = n
}

object WesternScale7 extends CreatesScaleWithModes[WesternScale7] {
  override protected def buildFromNamedIntervals(intervals: IndexedSeq[NamedInterval]): WesternScale7 =
    new WesternScale7(intervals)
}



class WesternScale12 protected ( override val intervals: IndexedSeq[NamedInterval] )
  extends ScaleWithModes(intervals)
  with WesternModes[WesternScale12] {

  private lazy val modeOffsets = Notes.tones map { Notes.tonesWithSharps.indexOf(_) }
  override protected def nthModeOffset(n: Int): Int = modeOffsets(n)
}

object WesternScale12 extends CreatesScaleWithModes[WesternScale12] {
  override protected def buildFromNamedIntervals(intervals: IndexedSeq[NamedInterval]): WesternScale12 =
    new WesternScale12(intervals)
}
