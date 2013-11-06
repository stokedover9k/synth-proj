package synth.sounds

import java.io.File
import javax.sound.sampled.{AudioSystem, AudioFormat}
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
