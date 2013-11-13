package synth.sounds

import synth.scales.{IntervalType, ScaleBuilderWesternHepto, ScaleBuilderPtolemyChromatic, ScaleBuilderZarlino}

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 11/12/13
 * Time: 4:34 PM
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
    for (i <- 0 until samples.size) samples(i) = Math.min(1, Math.max(-1, totalSamples(i)))
    new ComplexChord(tones, newSynths)
  }
}

object ComplexChord {

  def apply(tones: Seq[ComplexTone], sampleRate: Float = 44100f): ComplexChord = {
    new ComplexChord(tones, Array.fill(tones.size)(WavedToneSynthesiser(sampleRate = sampleRate)))
  }
}


object ChordPlayer {
  val toneComponents = Seq(
    ComplexTone.Component(1f, 1f),
    ComplexTone.Component(2f, 0.7f),
    ComplexTone.Component(3f, 0.4f),
    ComplexTone.Component(5f, 0.3f),
    ComplexTone.Component(6f, 0.15f),
    ComplexTone.Component(7f, 0.1f)
  )

  def main(args: Array[String]): Unit = {

    val line = getLine

    val buffsPerSec = 10
    val samples = new Array[Float]((SAMPLE_RATE / buffsPerSec).toInt)

    // prototype scale (not actually played)
    val scale0 = ScaleBuilderPtolemyChromatic(528f).build

    // scales to be played
    val scale1 = ScaleBuilderWesternHepto(scale0, IntervalType.getWholeSteps :+ IntervalType.First).build
    val scale2 = scale1.mode(3)
    val scale3 = scale1.mode(4)

    def tone(i: synth.Series.Interval) = ComplexTone(i.hz, toneComponents)

    (scale1.intervals, scale2.intervals, scale3.intervals).zipped.foreach {
      case (i1, i2, i3) => {

        val tones = Array(tone(i1), tone(i2), tone(i3))
        var chord: Chord = ComplexChord(tones)

        for (i <- 0 to buffsPerSec) {
          chord = chord.getSamples(samples)
          line.write(samples.map(y => (y * 64).toByte), 0, samples.size)
        }
      }
    }

    line.drain()
    line.stop()
    line.close()
  }
}
