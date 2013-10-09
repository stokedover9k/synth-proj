package synth.scales

import synth.NoteSeries.Interval

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
class ModeScale( override val intervals: Seq[Interval],
                   override val notes: Seq[String] )
  extends MyScale(intervals, notes)
  with Modes[ModeScale] {

  protected def noteToEndMapper: (String) => String = (s: String) => s

  protected def intervalToEndMapper: (Interval) => Interval = (i: Interval) => i.octaveUp

  /*
     * Takes sequence S and a cut index C and produces a pair of sequences (Sh, St)
     * where Sh is the head which gets remapped to the end of the sequence sequence
     * using remap***ToEnd and St becomes the unaltered beginning of the new mode.
     */
  protected def cutSeqForMode[T]: (Seq[T]) => (Int) => (Seq[T], Seq[T]) =
    (s: Seq[T]) => (n: Int) => s.splitAt(n)

  protected def buildScale: (Seq[Interval], Seq[String]) => ModeScale =
    (is, ns) => new ModeScale(is, ns)
}
