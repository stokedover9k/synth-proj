package synth

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 10/4/13
 * Time: 11:58 AM
 * To change this template use File | Settings | File Templates.
 */



class ToneSynthesiser private(
                               buffer: Array[Float],
                               val sampleRate: Float,
                               currentTime: Float
                               ) {

  lazy val resolution: Float = buffer.size

  private def sample(t: Float, f: Float): Float = {
    val ind: Float = t * f * resolution
    val a = ind.floor.toInt
    val b = a + 1
    buffer(a % buffer.length) * (b - ind) + buffer(b % buffer.length) * (ind - a)
  }

  def getSamples(tone: ComplexTone, samples: Array[Float]): ToneSynthesiser = {
    def timeOfStep(i: Int): Float = currentTime + i / sampleRate
    def componentHz(cmp: ComplexTone.Component): Float = tone.hz * cmp.hzRatio

    // for each sample i...
    0 until samples.size foreach {
      i => {
        // set sample i to sum over components of amplitude-adjusted samples
        samples(i) = (0f /: tone.components)(
          (sum: Float, cmp: ComplexTone.Component) =>
            sum + cmp.ampRatio * sample(timeOfStep(i), componentHz(cmp))
        )
      }
    }

    new ToneSynthesiser(buffer, sampleRate, timeOfStep(samples.size))
  }
}

object ToneSynthesiser {

  def apply(resolution: Int,
            sampleRate: Float = 44100,
            currentTime: Float = 0
             ): ToneSynthesiser = {

    val buffer = new Array[Float](resolution)

    val wave = Sine(1) // wave at 1hz

    0 until resolution foreach {
      i =>
        buffer(i) = wave.at(i.toFloat / resolution)
    }

    new ToneSynthesiser(buffer, sampleRate, currentTime)
  }
}

