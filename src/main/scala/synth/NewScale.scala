package synth

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 10/7/13
 * Time: 3:29 PM
 * To change this template use File | Settings | File Templates.
 */

class NewScale private(intervals: IndexedSeq[NoteSeries.Interval]) {

  def size = intervals.size

  def apply(index: Int): NoteSeries.Interval =
    if( index == intervals.size )
      intervals(0).octaveUp
    else
      intervals(index)

  def modeAt(offset: Int): NewScale = {
    val (h, t) = intervals.splitAt(offset)
    NewScale( t ++ (h map { _.octaveUp }) )
  }

  def octave: NoteSeries.Interval = apply(size)

  override def toString = intervals.mkString("[", " ", "]")
}

object NewScale {

  def apply(intervals: IndexedSeq[NoteSeries.Interval]): NewScale = {

    if( !(intervals, intervals.tail).zipped.forall( (a,b) => { a.hz <= b.hz && b.hz <= 2 * intervals(0).hz } ) )
      throw sys.error("frequencies must be in ordered and within an octave")

    new NewScale(intervals)
  }
}
