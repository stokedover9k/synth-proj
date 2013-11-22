package synth.sounds

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 11/13/13
 * Time: 12:59 PM
 * To change this template use File | Settings | File Templates.
 */

abstract class Chord {

  def tones: Seq[Tone]

  def getSamples(samples: Array[Float], num: Int = -1, offset: Int = 0): Chord
}


class ComplexChord protected(override val tones: Seq[ComplexTone], private val synthesizers: Seq[ToneSynthesiser])
  extends Chord {

  def getSamples(samples: Array[Float], num: Int = -1, offset: Int = 0): ComplexChord = {
    val n = if (num < 0) samples.size - offset else num
    val last = offset + n

    for (i <- offset until last)
      samples(i) = 0

    val tmpSamples = new Array[Float](n)

    val newSynths = (tones, synthesizers).zipped.map {
      case (t, s) => {
        val newSynth = s.getSamples(t, tmpSamples, num = n, offset = 0)
        tmpSamples.zipWithIndex.foreach {
          case (v, i) => samples(offset + i) = samples(offset + i) + v
        }
        newSynth
      }
    }
    for (i <- offset until last)
      samples(i) = Math.min(1, Math.max(-1, samples(i)))
    new ComplexChord(tones, newSynths)
  }
}


object ComplexChord {

  def apply(tones: Seq[ComplexTone], sampleRate: Float = 44100f): ComplexChord = {
    new ComplexChord(tones, Array.fill(tones.size)(WavedToneSynthesiser(sampleRate = sampleRate)))
  }
}

