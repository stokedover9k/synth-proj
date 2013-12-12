package synth.models

import javax.sound.sampled.AudioFormat
import synth.sounds.util.{Formats, SoundStream}
import synth.scales.TypedScale
import synth.sounds.{ComplexChord, ComplexTone}
import java.util.concurrent.atomic.AtomicReference

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 12/8/13
 * Time: 10:11 PM
 * To change this template use File | Settings | File Templates.
 */
object DemoChordPlayer {

  lazy val toneComponents = synth.sounds.ChordPlayer.toneComponents

  val SAMPLE_RATE = synth.sounds.SAMPLE_RATE
  lazy val outFormat = new AudioFormat(SAMPLE_RATE, 8, 1, true, true)
  lazy val line = synth.sounds.getLine(outFormat)

  lazy val player = SoundStream.streamPlayerFor(line)(SAMPLE_RATE.toInt / 8)

  def demoPlay(scale: TypedScale, song: List[String]): Unit = {
    val s: List[(String, Float)] = song map (n => (n, .5f))
    lazy val chordFrames = s map {
      case (note: String, duration: Float) =>
        ComplexChord(Seq(ComplexTone(scale(note).hz, toneComponents))) -> (duration * SAMPLE_RATE).toInt
    }
    demoPlay(chordFrames)
  }

  def demoPlayChords(scale: TypedScale, song: List[Seq[String]]): Unit = {
    def tones(notes: Seq[String]) = notes map {
      note => ComplexTone(scale(note).hz, toneComponents)
    }
    def chords = song map {
      chord => ComplexChord(tones(chord)) -> (.5f * SAMPLE_RATE).toInt
    }
    demoPlay(chords)
  }

  def demoPlayChordsSeconds(scale: TypedScale, song: List[(Seq[String], Int)]): Unit = {
    def tones(notes: Seq[String]) = notes map {
      note => ComplexTone(scale(note).hz, toneComponents)
    }
    def chords = song map {
      chord => ComplexChord(tones(chord._1)) -> (chord._2 * .5f * SAMPLE_RATE).toInt
    }
    demoPlay(chords)
  }

  def demoPlay(chordFrames: List[(ComplexChord, Int)]): Unit = {
    def songStream =
      SoundStream.getSampleStream(chordFrames).map {
        Formats.floatConverter(line.getFormat)
      }.flatten

    player(new AtomicReference(songStream), -1)
    line.drain()
  }

  def demoPlaySeconds(chords: List[(ComplexChord, Int)]): Unit = {
    def chordFrames = chords map {
      case (chord: ComplexChord, seconds: Int) =>
        (chord, (seconds * SAMPLE_RATE).toInt)
    }
    demoPlay(chordFrames)
  }
}
