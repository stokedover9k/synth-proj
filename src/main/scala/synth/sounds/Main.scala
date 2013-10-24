package synth.sounds

import javax.sound.sampled._

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 9/13/13
 * Time: 8:32 PM
 * To change this template use File | Settings | File Templates.
 */
object Main {

  val SAMPLE_RATE: Float = 44100

  def getLine: SourceDataLine = {
    val af: AudioFormat = new AudioFormat( SAMPLE_RATE, 8, 1, true, false )
    var sdl: SourceDataLine = AudioSystem.getSourceDataLine( af )
    sdl = AudioSystem.getSourceDataLine( af )
    sdl.open( af )
    sdl.start()
    sdl
  }

  def main(args: Array[String]) = {

    var os: Oscillator = BufferedWaveOscillator(1024, Sine, SAMPLE_RATE, 0)
    val samples = new Array[Float](2048)
    val bufferTimeLength = samples.size / SAMPLE_RATE

    val line = getLine

    val repeatBuffers = (3/bufferTimeLength).toInt
    println("repeating buffer " + repeatBuffers + " times")
    for( i <- 0 until repeatBuffers ) {
      os = os.getSamples(440, samples)
      line.write(samples.map( y => (y*0.7*64).toByte), 0, samples.size)
    }

    line.drain()
    line.stop()
    line.close()
  }

}
