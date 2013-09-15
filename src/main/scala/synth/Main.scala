package synth

import javax.sound.sampled._

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 9/13/13
 * Time: 8:32 PM
 * To change this template use File | Settings | File Templates.
 */
object Main {

//  val SINE_SAMPLES = 1024
//
//  val sineWave: Array[Float] = {
//    val a = new Array[Float](SINE_SAMPLES)
//    0 until SINE_SAMPLES foreach { x => a(x) = Math.sin( x.toFloat / SINE_SAMPLES * (2 * Math.PI) ).toFloat }
//    a
//  }
//
//  def main(args: Array[String]) = {
//
//    val format : AudioFormat = new AudioFormat(44100f, 8, 1, true, false)
//    val info: DataLine.Info = new DataLine.Info(classOf[SourceDataLine], format)
//    if( !AudioSystem.isLineSupported(info) ) {
//      throw sys.error("line not supported")
//    }
//    val line: SourceDataLine = try {
//      val line: SourceDataLine = AudioSystem.getLine(info).asInstanceOf[SourceDataLine]
//      line.open(format)
//      line
//    }
//
//    for( i <- 1 to 50000 )
//      line.write(sineWave.map( y => (y*63).toByte ), 0, sineWave.size)
//
//    line.drain()
//    line.stop()
//    line.close()
//
//    println("hello")
//  }

  val SAMPLE_RATE: Double = 44100

  def getLine: SourceDataLine = {
    val buf = new Array[Byte](1)
    val af: AudioFormat = new AudioFormat( SAMPLE_RATE.toFloat, 8, 1, true, false )
    var sdl: SourceDataLine = AudioSystem.getSourceDataLine( af )
    sdl = AudioSystem.getSourceDataLine( af )
    sdl.open( af )
    sdl.start()
    sdl
  }

  def getWave(f: Double) = {
    val l = 1d / f
    val sr = SAMPLE_RATE

    val ar =
      (for( x <- 0 until SAMPLE_RATE.toInt ) yield Math.sin(x / sr * f * 2 * Math.PI))
      .toArray.map( x => x * 0.7 * 63 ).map( x => x.toByte )

    ar
  }

  def main(args: Array[String]) = {

    var os: Oscillator = new SoundWaveOscillator(SAMPLE_RATE)
    val samples = new Array[Double](2048)
    os = os.getSamples(440, samples)
    val bufferTimeLength = samples.size / SAMPLE_RATE

    val line = getLine

    val repeatBuffers = (5/bufferTimeLength).toInt
    println("repeating buffer " + repeatBuffers + " times")
    for( i <- 0 until repeatBuffers )
      line.write(samples.map( y => (y*0.7*64).toByte), 0, samples.size)

    line.drain()
    line.stop()
    line.close()
  }

}
