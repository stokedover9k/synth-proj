package synth

abstract class Oscillator {

  val sampleRate: Float

  /*
   * Fills the n elements of the samples array starting at offset with samples at the specified frequency
   * @param f Frequency.
   * @param samples Array of samples which is modified for output.
   * @param n Number of elements to fill. If n < 0, fill to the end of the samples array. It is the caller's responsibility to take care of the array's boundary conditions.
   * @param offset Offset at which to start filling the samples array.
   */
  def getSamples(f: Float, samples: Array[Float], n: Int = -1, offset: Int = 0): Oscillator
}



case class WaveOscillator ( sampleRate: Float = 44100,
                            currentTime: Float = 0,
                            waves: CreatesWaves = Sine
                          ) extends Oscillator
{
  def getSamples(f: Float, samples: Array[Float], n: Int = -1, offset: Int = 0): WaveOscillator = {

    val top = if( n < 0 ) samples.size else offset + n
    val num = top - offset
    val wave = waves(f)
    def timeOfStep(i: Int): Float = currentTime + i / sampleRate

    for( i <- 0 until num ) { samples(offset + i) = wave.at(timeOfStep(i)) }

    new WaveOscillator(sampleRate, timeOfStep(num), waves)
  }
}



case class BufferedWaveOscillator ( sampleRate: Float = 44100,
                                    currentTime: Float = 0,
                                    buffer: IndexedSeq[Float],
                                    resolution: Float
                                  ) extends Oscillator
{
  def sample(t: Float, f: Float): Float = {

    val ind = t * f * resolution
    val a = Math.floor(ind).toInt
    val b = (a + 1)

    buffer(a % buffer.length) * (b - ind) + buffer(b % buffer.length) * (ind - a)
  }

  def getSamples(f: Float, samples: Array[Float], n: Int, offset: Int): Oscillator = {

    val top = if( n < 0 ) samples.size else offset + n
    val num = top - offset
    def timeOfStep(i: Int): Float = currentTime + i / sampleRate

    for( i <- 0 until num ) { samples(offset + i) = sample(timeOfStep(i), f) }

    new BufferedWaveOscillator(sampleRate, timeOfStep(num), buffer, resolution)
  }
}

object BufferedWaveOscillator {

  def apply( resolution: Int
             , waves: CreatesWaves
             , sampleRate: Float
             , currentTime: Float
             ): BufferedWaveOscillator =
  {
    val wave = waves(1)  // wave at frequency 1 Hz
    val buffer = new Array[Float](resolution)
    for( i <- 0 until resolution )
      buffer(i) = wave.at(i.toFloat / resolution)

    new BufferedWaveOscillator(sampleRate, currentTime, buffer, resolution)
  }
}