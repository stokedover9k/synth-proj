package synth.oldscales

import synth.NoteSeries.Interval
import synth.NoteSeries

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 10/7/13
 * Time: 11:28 AM
 * To change this template use File | Settings | File Templates.
 */

/*
 * 7-tone scale.
 * Addressable notes are at indices 0 to 7 (7th note is the octave).
 */
class Scale7(protected val frequencies: IndexedSeq[Interval]) extends Scale {
  def size: Int = 7
  def apply(index: Int): Interval = if( index == size ) frequencies(0).octaveUp else frequencies(index)

  def mode(offset: Int): Scale7 = {
    val arr = new Array[Interval](size)
    (frequencies drop offset).copyToArray(arr, 0, size - offset)
    (frequencies take offset).map(_.octaveUp).copyToArray(arr, size - offset)
    new Scale7(arr)
  }

  def Ionian = mode(0)
  def Dorian = mode(1)
  def Phrygian = mode(2)
  def Lydian = mode(3)
  def Mixolydian = mode(4)
  def Aeolian = mode(5)
  def Locrian = mode(6)
  def IonianOctave = mode(7)
}

object Scale7 {
  def apply(frequencies: IndexedSeq[Interval]): Scale7 = {
    if( frequencies.size != 7 )
      throw sys.error("expected 7 frequencies")
    if( !(frequencies, frequencies.tail).zipped.forall( (a,b) => { a.hz <= b.hz && b.hz <= 2 * frequencies(0).hz } ) )
      throw sys.error("frequencies must be in ordered and within an octave")
    new Scale7(frequencies)
  }
}

abstract class Series2Scale7[S <: NoteSeries] {

  /*
   * Returns the first 7 notes of the scale.  Assumed to be sorted by frequency.
   */
  def sorted7(s: S): IndexedSeq[Interval]

  def buildScale7(s: S): Scale7 = Scale7(sorted7(s))
}
