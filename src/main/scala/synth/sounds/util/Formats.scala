package synth.sounds.util

import javax.sound.sampled.AudioFormat

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 11/19/13
 * Time: 1:09 PM
 * To change this template use File | Settings | File Templates.
 */

object Formats {

  def floatConverter(af: AudioFormat): Float => Array[Byte] = {
    def byte(f: Float) = (f * 0x7E).toByte
    def max2(f: Float) = ((f * 0x7FFF).toInt >> 8).toByte
    def min2(f: Float) = ((f * 0x7FFF).toInt & 0xFF).toByte

    (af.getSampleSizeInBits, af.getChannels, af.isBigEndian) match {
      case (8, 1, true) | (8, 1, false) =>
        f => Array(byte(f))
      case (16, 1, true) =>
        f => Array(max2(f), min2(f))
      case (16, 1, false) =>
        f => Array(min2(f), max2(f))
      case (8, 2, true) | (8, 2, false) =>
        f => Array(byte(f), byte(f))
      case (16, 2, true) =>
        f => Array(max2(f), min2(f), max2(f), min2(f))
      case (16, 2, false) =>
        f => Array(min2(f), max2(f), min2(f), max2(f))
      case _ =>
        throw sys.error("requesting converter for unknown format %s".format(af.toString))
    }
  }
}
