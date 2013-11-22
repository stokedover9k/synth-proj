package synth.sounds.util

import javax.sound.sampled.{SourceDataLine, AudioFormat}
import synth.sounds._
import synth.scales.ScaleBuilderMeanTone
import java.util.concurrent.atomic.AtomicReference

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 11/18/13
 * Time: 4:31 PM
 * To change this template use File | Settings | File Templates.
 */

object SoundStream {

  /*
   * @arg chords contains tuples like (chord, number of remaining frames).
   */
  def getSampleStream(timedChords: List[(ComplexChord, Int)], CHORD_BUFFER_SIZE: Int = 1024 * 4): Stream[Float] = {

    val buffer = new Array[Float](CHORD_BUFFER_SIZE)

    def loop(chords: List[(ComplexChord, Int)]): Stream[Float] = chords match {
      case Nil => Stream()
      case (chord, frames) :: tail => {

        if (CHORD_BUFFER_SIZE < frames) {
          val newChord = chord.getSamples(buffer, CHORD_BUFFER_SIZE, 0)
          buffer.iterator.toStream append loop((newChord, frames - CHORD_BUFFER_SIZE) :: tail)
        } else {
          chord.getSamples(buffer, frames, 0)
          buffer.take(frames).toStream append loop(tail)
        }
      }
    }

    loop(timedChords)
  }

  /*
   * Note that the stream is wrapped into a reference object so that its head can be garbage collected
   * (otherwise, the original object remains on the stack and prevents cleanup).
   */
  private def streamPlayer = (line: SourceDataLine) => (framesPerBuffer: Int) => (streamRef: AtomicReference[Stream[Byte]], frames: Int) => {

    val bytesPerFrame = line.getFormat.getFrameSize

    def play(streamRef: AtomicReference[Stream[Byte]], frames: Int): Stream[Byte] = {
      val (n, df) =
        if (frames == -1) (framesPerBuffer * bytesPerFrame, 0)
        else (Math.min(frames, framesPerBuffer) * bytesPerFrame, -Math.min(frames, framesPerBuffer))

      if (frames > 0 || frames == -1) {
        val (head, tail) = streamRef.getAndSet(null).splitAt(n)
        val samples = head.toArray
        line.write(samples, 0, samples.size)
        if (tail.nonEmpty)
          play(new AtomicReference(tail), frames + df)
        else
          tail
      }
      else
        streamRef.getAndSet(null)
    }

    play(new AtomicReference[Stream[Byte]](streamRef.getAndSet(null)), frames)
  }

  def streamPlayerFor(line: SourceDataLine)(framesPerBuffer: Int) = streamPlayer(line)(framesPerBuffer)(_, _)
}

object SoundStreamTest extends App {

  val SAMPLE_RATE = synth.sounds.SAMPLE_RATE

//  val toneComponents = Seq(ComplexTone.Component(1f, 1f))
    val toneComponents = synth.sounds.ChordPlayer.toneComponents

  val scale = ScaleBuilderMeanTone(528f).build

  val song: List[(String, Float)] = {
    val notes =         "E,D,C,D,E,E,E,D,D,D,E,G,G,E,D,C,D,E,E,E,E,D,D,E,D,C".split(",").toList
    val durations = List(1,1,1,1,1,1,2,1,1,2,1,1,2,1,1,1,1,1,1,1,1,1,1,1,1,4) map (_ * .5f)
    notes zip durations
  }

  val chordFrames = song ++ song ++ song ++ song map {
    case (note: String, duration: Float) =>
      ComplexChord(Seq(ComplexTone(scale(note).hz, toneComponents))) -> (duration * SAMPLE_RATE).toInt
  }

  val outFormat = new AudioFormat(SAMPLE_RATE, 8, 1, true, true)

  val line = synth.sounds.getLine(outFormat)

  val player = SoundStream.streamPlayerFor(line)(SAMPLE_RATE.toInt / 8)

  def songStream = SoundStream.getSampleStream(chordFrames) map (Formats.floatConverter(line.getFormat)) flatten

  player(new AtomicReference(songStream), -1)

  line.drain()
  line.stop()
  line.close()
}
