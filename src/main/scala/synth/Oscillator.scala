package synth

abstract class Oscillator {

  val sampleRate: Double

  /*
   * Fills the n elements of the samples array starting at offset with samples at the specified frequency
   * @param f Frequency.
   * @param samples Array of samples which is modified for output.
   * @param n Number of elements to fill. If n < 0, fill to the end of the samples array. It is the caller's responsibility to take care of the array's boundary conditions.
   * @param offset Offset at which to start filling the samples array.
   */
  def getSamples(f: Double, samples: Array[Double], n: Int = -1, offset: Int = 0): Oscillator
}



case class SoundWaveOscillator(sampleRate: Double = 44100, currentPosition: SoundWave = new SineWave(0)) extends Oscillator {

  def getSamples(f: Double, samples: Array[Double], n: Int = -1, offset: Int = 0): Oscillator = {

    val top = if( n < 0 ) samples.size else offset + n
    def stepsPerWave = sampleRate / f
    val stepSize = 2 * Math.PI / stepsPerWave
    var p: SoundWave = currentPosition

    for( i <- offset until top ) { samples(i) = p.amp;  p = p.next(stepSize) }

    SoundWaveOscillator(sampleRate, p)
  }
}
