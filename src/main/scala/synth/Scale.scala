package synth

import scala.util.Sorting

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 9/30/13
 * Time: 7:25 PM
 * To change this template use File | Settings | File Templates.
 */

abstract class Scale {
  def size: Int
  def apply(index: Int): Float

  override def toString = (0 until size).map( i => apply(i) ).addString(new StringBuilder, "[", " ", "]").toString
}

/*
 * 7-note scale.
 * Addressable notes are at indices 0 to 7 (7th note is the octave).
 */
class Scale7(private val frequencies: IndexedSeq[Float]) extends Scale {
  def size: Int = 7
  def apply(index: Int): Float = if( index == 7 ) frequencies(0) * 2 else frequencies(index)

  def mode(offset: Int): Scale7 = {
    val arr = new Array[Float](size)
    (frequencies drop offset).copyToArray(arr, 0, size - offset)
    (frequencies take offset).map(_ * 2).copyToArray(arr, size - offset)
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
  def apply(frequencies: IndexedSeq[Float]): Scale7 = {
    if( frequencies.size != 7 )
      throw sys.error("expected 7 frequencies")
    if( !(frequencies, frequencies.tail).zipped.forall( (a,b) => { a <= b && b <= 2 * frequencies(0)} ) )
      throw sys.error("frequencies must be in ordered and within an octave")
    new Scale7(frequencies)
  }
}

abstract class Series2Scale7[S <: NoteSeries] {

  def extract7(s: S): Iterable[Float]

  def buildScale(s: S): Scale7 = {
    val frequencies = extract7(s).toArray[Float]
    Sorting.quickSort(frequencies)
    Scale7(frequencies)
  }
}
