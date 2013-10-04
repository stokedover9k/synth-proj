package synth

import scala.util.Sorting
import synth.NoteSeries.Interval

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 9/30/13
 * Time: 7:25 PM
 * To change this template use File | Settings | File Templates.
 */

abstract class Scale {
  def size: Int
  def apply(index: Int): NoteSeries.Interval

  override def toString = (0 until size).map( i => apply(i) ).addString(new StringBuilder, "[", " ", "]").toString
}

/*
 * 7-note scale.
 * Addressable notes are at indices 0 to 7 (7th note is the octave).
 */
class Scale7(private val frequencies: IndexedSeq[Interval]) extends Scale {
  def size: Int = 7
  def apply(index: Int): Interval = if( index == 7 ) frequencies(0).inNextOctave else frequencies(index)

  def mode(offset: Int): Scale7 = {
    val arr = new Array[Interval](size)
    (frequencies drop offset).copyToArray(arr, 0, size - offset)
    (frequencies take offset).map(_.inNextOctave).copyToArray(arr, size - offset)
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

  def extract7(s: S): Iterable[Interval]

  def buildScale(s: S): Scale7 = {
    val intervals = extract7(s).toArray[Interval]
    Scale7(intervals.sortBy(_.hz))
  }
}
