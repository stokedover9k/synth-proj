package synth.scales

import synth.NoteSeries.Interval

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
    buildScale(
      iTail ++ (iHead map intervalToEndMapper),
      nTail ++ (nHead map noteToEndMapper)
    )
  }

  protected def noteToEndMapper: String => String
  protected def intervalToEndMapper: Interval => Interval

  /*
   * Takes sequence S and a cut index C and produces a pair of sequences (Sh, St)
   * where Sh is the head which gets remapped to the end of the sequence sequence
   * using remap***ToEnd and St becomes the unaltered beginning of the new mode.
   */
  protected def cutSeqForMode[T]: Seq[T] => Int => (Seq[T], Seq[T])

  protected def buildScale: (Seq[Interval], Seq[String]) => S

  private def intervalCutter = cutSeqForMode(intervals)
  private def noteCutter = cutSeqForMode(notes)
}
