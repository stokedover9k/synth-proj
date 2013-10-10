package synth.scales

import synth.scale.Note
import synth.NoteSeries.Interval

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 10/10/13
 * Time: 1:42 PM
 * To change this template use File | Settings | File Templates.
 */

//class StepScale(override val intervals: Seq[Interval],
//
//                override val notes: Seq[Note],
//                override val stepPattern: Seq[Int])
//  extends MyScale(intervals, notes)
//  with Modes[StepScale]
//  with ScaleWithSteps[StepScale] {
//
//  /*
//   * Takes sequence S and a cut number C and produces a pair of sequences (Sh, St)
//   * where Sh is the sub-sequence which gets remapped to the next octave and added
//   * to the end of the new mode (after St).
//   */
//  protected def cutSeqForMode[T]: (Seq[T]) => (Int) => (Seq[T], Seq[T]) =
//    (s: Seq[T]) => (n: Int) => s.splitAt(n)
//
////  def buildScale: (Seq[Interval], Seq[Note]) => StepScale =
////    (is, ns) => buildScale(stepPattern)(is, ns)
////
////  def buildScale: Seq[Int] => (Seq[Interval], Seq[Note]) => StepScale =
////    ss => (is, ns) => new StepScale(is, ns, ss)
//}
