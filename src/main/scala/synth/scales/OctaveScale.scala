package synth.scales

import synth.scale.Note
import synth.NoteSeries.Interval

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 10/10/13
 * Time: 1:45 AM
 * To change this template use File | Settings | File Templates.
 */

class OctaveScale(override val intervals: Seq[Interval],
                  override val notes: Seq[Note])
  extends MyScale(intervals, notes)
  with Modes[OctaveScale] {

  /*
   * Takes sequence S and a cut number C and produces a pair of sequences (Sh, St)
   * where Sh is the sub-sequence which gets remapped to the next octave and added
   * to the end of the new mode (after St).
   */
  protected def cutSeqForMode[T]: (Seq[T]) => (Int) => (Seq[T], Seq[T]) =
    (s: Seq[T]) => (n: Int) => s.tail.splitAt(n)

  protected def buildScaleFromIntervalsAndNotes: (Seq[Interval], Seq[Note]) => OctaveScale =
    (is, ns) => new OctaveScale(is, ns)
}



