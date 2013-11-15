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

  def getSamples(samples: Array[Float]): Chord
}


class ComplexChord protected(override val tones: Seq[ComplexTone], private val synthesizers: Seq[ToneSynthesiser])
  extends Chord {

  def getSamples(samples: Array[Float]): ComplexChord = {
    val tmpSamples = new Array[Float](samples.size)
    val totalSamples = new Array[Float](samples.size)
    val newSynths = (tones, synthesizers).zipped.map {
      case (t, s) => {
        val newSynth = s.getSamples(t, tmpSamples)
        tmpSamples.zipWithIndex.foreach {
          case (v, i) => totalSamples(i) = totalSamples(i) + v
        }
        newSynth
      }
    }
    for (i <- 0 until samples.size)
      samples(i) = Math.min(1, Math.max(-1, totalSamples(i)))
    new ComplexChord(tones, newSynths)
  }
}


object ComplexChord {

  def apply(tones: Seq[ComplexTone], sampleRate: Float = 44100f): ComplexChord = {
    new ComplexChord(tones, Array.fill(tones.size)(WavedToneSynthesiser(sampleRate = sampleRate)))
  }
}

