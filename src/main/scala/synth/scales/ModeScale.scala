package synth.scales

import synth.NoteSeries.Interval
import synth.scale.Note

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 10/9/13
 * Time: 1:50 PM
 * To change this template use File | Settings | File Templates.
 */
/*
 * This scale assumes and guarantees nothing about the notes it holds.  To produce modes,
 * the notes are cut at indicated index and wrapped around.
 * DO NOT use this class for scales whose last note is equivalent to (octave of) the first.
 */
class ModeScale(override val intervals: Seq[Interval],
                override val notes: Seq[Note])
  extends MyScale(intervals, notes)
  with Modes[ModeScale] {

  /*
   * Takes sequence S and a cut number C and produces a pair of sequences (Sh, St)
   * where Sh is the sub-sequence which gets remapped to the next octave and added
   * to the end of the new mode (after St).
   */
protected def cutSeqForMode[T]: (Seq[T]) => (Int) => (Seq[T], Seq[T]) =
    (s: Seq[T]) => (n: Int) => s.splitAt(n)

  protected def buildScaleFromIntervalsAndNotes: (Seq[Interval], Seq[Note]) => ModeScale =
    (is, ns) => new ModeScale(is, ns)
}
