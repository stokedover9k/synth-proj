package synth.scales

import synth.NoteSeries.Interval
import synth.scale.Note

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 10/9/13
 * Time: 1:22 PM
 * To change this template use File | Settings | File Templates.
 */

trait Modes[S <: ScaleInterface] extends ScaleInterface {

  def mode(offset: Int): S = {
    val (iHead, iTail) = intervalCutter(offset)
    val (nHead, nTail) = noteCutter(offset)
    buildScaleFromIntervalsAndNotes(
      iTail ++ (iHead map (_.octaveUp)),
      nTail ++ (nHead map (_.octaveUp))
    )
  }

  /*
   * Takes sequence S and a cut number C and produces a pair of sequences (Sh, St)
   * where Sh is the sub-sequence which gets remapped to the next octave and added
   * to the end of the new mode (after St).
   */
  protected def cutSeqForMode[T]: Seq[T] => Int => (Seq[T], Seq[T])

  protected def buildScaleFromIntervalsAndNotes: (Seq[Interval], Seq[Note]) => S

  private def intervalCutter = cutSeqForMode(intervals)
  private def noteCutter = cutSeqForMode(notes)
}
