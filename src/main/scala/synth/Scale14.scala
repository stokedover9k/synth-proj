package synth

import synth.NoteSeries.Interval

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 10/7/13
 * Time: 11:29 AM
 * To change this template use File | Settings | File Templates.
 */

class Scale14(protected override val frequencies: IndexedSeq[Interval])
  extends Scale7(frequencies) {

  override def size = 14

  override def Ionian = mode(0)
  override def Dorian = mode(2)
  override def Phrygian = mode(4)
  override def Lydian = mode(5)
  override def Mixolydian = mode(8)
  override def Aeolian = mode(10)
  override def Locrian = mode(12)
  override def IonianOctave = mode(13)
}

object Scale14 {
  def apply(frequencies: IndexedSeq[Interval]): Scale14 = {
    if( frequencies.size != 14 )
      throw sys.error("expected 14 frequencies")
    if( !(frequencies, frequencies.tail).zipped.forall(
      (a,b) => {
        a.hz <= b.hz && b.hz <= 2 * frequencies(0).hz
      }
    ) )
      throw sys.error("frequencies must be in ordered and within an octave")
    new Scale14(frequencies)
  }
}

abstract class Series2Scale14[S <: NoteSeries] {

  /*
   * Returns the first 14 intervals of the series.  Assumed to be sorted by frequency.
   */
  def sorted14(s: S): IndexedSeq[Interval]

  def buildScale14(s: S) = Scale14(sorted14(s))
}