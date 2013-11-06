package synth.sounds.rhythm

import javax.sound.sampled._
import java.io.{ByteArrayInputStream, File}
import scala.collection.mutable
import util.expr.{Expr, Div, Fraction}
import synth.sounds.{AudioBytesArrays, ClipCollection}

object DrumsDemo {

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
  val sampleFile1 = "CYCdh_K1close_Flam-03.wav"
  val sampleFile2 = "CYCdh_K1close_OpHat-05.wav"

  val drumFileNames = Seq(
    "CYCdh_K1close_Rim-01.wav",
    "CYCdh_K1close_Flam-03.wav",
    "CYCdh_K1close_OpHat-05.wav",
    "CYCdh_K1close_SnrOff-06.wav",
    "CYCdh_K1close_ClHat-05.wav",
    "CYCdh_K1close_ClHat-07.wav",
    "CYCdh_K1close_OpHat-03.wav",
    "CYCdh_K1close_OpHat-07.wav"
  )

  def main(args: Array[String]): Unit = {

    val outDataFormat = new AudioFormat(SAMPLE_RATE, 8, 1, true, false)

    val line = getLine(outDataFormat)

    val tempo = 160

    var cc = drumFileNames.map {
      // map to files
      filename => new File(kitDir + filename)
    }.map {
      // map to sample arrays
      drumFile => AudioBytesArrays.fromFile(drumFile, outDataFormat)
    }.zipWithIndex.map {
      // map to drum patterns
      case (drumSamples, index) => new DrumPattern(DrumPatterns.afrikanPattern, drumSamples) mode index
    }.map {
      // map to clip collection
      _.getClipsAt(tempo, SAMPLE_RATE)
    }.foldLeft(ClipCollection()) {
      // add all clip collections together
      _ ++ _
    }

    val audioBytes = new Array[Byte](1024)

    while (!cc.isEmpty) {
      cc = cc.getSamples(audioBytes)
      line.write(audioBytes, 0, audioBytes.length)
    }

    line.drain()
    line.stop()
    line.close()
  }
}
