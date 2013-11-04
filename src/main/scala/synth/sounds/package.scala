package synth

import javax.sound.sampled.{AudioFormat, SourceDataLine, AudioSystem}

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 11/4/13
 * Time: 11:51 AM
 * To change this template use File | Settings | File Templates.
 */
package object sounds {

  val SAMPLE_RATE: Float = 44100

  def getLine: SourceDataLine = {
    val af: AudioFormat = new AudioFormat( SAMPLE_RATE, 8, 1, true, false )
    var sdl: SourceDataLine = AudioSystem.getSourceDataLine( af )
    sdl = AudioSystem.getSourceDataLine( af )
    sdl.open( af )
    sdl.start()
    sdl
  }
}
