package synth.sounds

import javax.sound.sampled._
import java.io.{ByteArrayInputStream, File}
import scala.collection.mutable

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 11/4/13
 * Time: 12:30 PM
 * To change this template use File | Settings | File Templates.
 */

object AudioBytesArrays {

  def fromFile(inFile: File, outFormat: AudioFormat): Array[Byte] = {
    val inAIS = AudioSystem.getAudioInputStream(inFile)
    val outAIS = AudioSystem.getAudioInputStream(outFormat, inAIS)

    val bytesPerFrame = outAIS.getFormat.getFrameSize match {
      case AudioSystem.NOT_SPECIFIED => 1
      case v => v
    }

    val audioBytes = new Array[Byte](1024 * bytesPerFrame)
    var segments = new mutable.ArrayBuilder.ofByte
    while (outAIS.read(audioBytes) != -1) {
      segments ++= audioBytes
    }

    segments.result()
  }
}


object ClipCollection {

  protected case class Clip(begin: Int, bytes: Array[Byte]) {
    def end: Int = begin + bytes.length
  }

  def apply(currentOffset: Int = 0): ClipCollection =
    new ClipCollection(Set[ClipCollection.Clip](), currentOffset)

  def apply(clips: Set[ClipCollection.Clip], currentOffset: Int): ClipCollection =
    new ClipCollection(clips.filter(_.end > currentOffset), currentOffset)
}

class ClipCollection protected(private val clips: Set[ClipCollection.Clip] = Set[ClipCollection.Clip](),
                               val currentOffset: Int = 0) {

  def isEmpty: Boolean = clips.isEmpty

  def addClip(bytes: Array[Byte], offset: Int): ClipCollection = {
    val clip = ClipCollection.Clip(offset, bytes)
    if (clip.end <= currentOffset)
      this
    else
      new ClipCollection(clips + clip, currentOffset)
  }

  def addClip(bytes: Array[Byte]): ClipCollection = addClip(bytes, currentOffset)

  protected def getSample(offset: Int): Byte = {
    clips.foldLeft(0)((sum, clip) => {
      if (clip.begin <= offset && clip.end > offset)
        sum + clip.bytes(offset - clip.begin)
      else
        sum
    }) match {
      case x if x < Byte.MinValue => Byte.MinValue
      case x if x > Byte.MaxValue => Byte.MaxValue
      case x => x.toByte
    }
  }

  def getSamples(bytes: Array[Byte], n: Int = -1, offset: Int = 0): ClipCollection = {
    val top = if (n < 0) bytes.length else offset + n
    val num = top - offset

    for (i <- 0 until num) {
      bytes(offset + i) = getSample(currentOffset + i)
    }

    ClipCollection(clips, currentOffset + num)
  }
}


object Drums {

  val SAMPLE_RATE: Float = 44100

  def getLine(format: AudioFormat): SourceDataLine = {
    val af: AudioFormat = format
    var sdl: SourceDataLine = AudioSystem.getSourceDataLine(af)
    sdl = AudioSystem.getSourceDataLine(af)
    sdl.open(af)
    sdl.start()
    sdl
  }

  val kitDir = "src/main/resources/musicradar-drum-samples/Drum Kits/Kit 1 - Acoustic close/"
  val sampleFile = "CYCdh_K1close_Flam-03.wav"

  def main(args: Array[String]): Unit = {

    val outDataFormat = new AudioFormat(SAMPLE_RATE, 8, 1, true, false)

    val line = getLine(outDataFormat)

    val bytes = AudioBytesArrays.fromFile(new File(kitDir + sampleFile), outDataFormat)

    var clips = ClipCollection().addClip(bytes).addClip(bytes, 30000)

    val audioBytes = new Array[Byte](1024)

    while (!clips.isEmpty) {
      clips = clips.getSamples(audioBytes)
      line.write(audioBytes, 0, audioBytes.length)
    }

    line.drain()
    line.stop()
    line.close()
  }
}
