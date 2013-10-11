package synth.sounds

import synth.sounds.{ComplexTone, Sine, CreatesWaves, Wave}

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 10/4/13
 * Time: 11:58 AM
 * To change this template use File | Settings | File Templates.
 */

abstract class ToneSynthesiser {

  /*
   * Sampling rate.
   */
  val sampleRate: Float

  /*
   * Gets the ith sample from the current state for a pure tone at frequency f.
   */
  protected def sample(i: Int, f: Float): Float

  /*
   * Returns a synthesiser which is the same as this, but at the state after getting
   * the specified samples.
   */
  protected def afterSampling(tone: => ComplexTone, samples: => Array[Float]): ToneSynthesiser

  /*
   * Fills up the sample buffers with amplitude samples and returns a new synthesiser
   * in the post-sampling state.
   */
  def getSamples(tone: ComplexTone, samples: Array[Float]): ToneSynthesiser = {
    def hz(cmp: ComplexTone.Component): Float = tone.hz * cmp.hzRatio

    0 until samples.size foreach {
      i =>
        samples(i) = (0f /: tone.components)(
          (sum: Float, cmp: ComplexTone.Component) =>
            sum + cmp.ampRatio * sample(i, hz(cmp))
        )
    }

    afterSampling(tone, samples)
  }
}



class WavedToneSynthesiser private(
                                    wave: Wave,
                                    override val sampleRate: Float,
                                    currentTime: Float
                                    ) extends ToneSynthesiser {

  private def timeOfStep(i: Int): Float = currentTime + i / sampleRate

  /*
   * Gets the ith sample from the current state for a pure tone at frequency f.
   */
  override protected def sample(i: Int, f: Float): Float = wave.at(timeOfStep(i) * f)

  /*
   * Returns a synthesiser which is the same as this, but at the state after getting
   * the specified samples
   */
  protected def afterSampling(tone: => ComplexTone, samples: => Array[Float]): ToneSynthesiser =
    new WavedToneSynthesiser(wave, sampleRate, timeOfStep(samples.size))
}

object WavedToneSynthesiser {

  def apply(waveBuilder: CreatesWaves = Sine,
            sampleRate: Float = 44100f,
            currentTime: Float = 0f): WavedToneSynthesiser = {

    new WavedToneSynthesiser(waveBuilder(1f), sampleRate, currentTime)
  }
}

/*
 * This synthesiser seems to produce unpleasant sound artifacts, so use carefully.
 */
//class BufferedToneSynthesiser private(
//                                       buffer: Array[Float],
//                                       val sampleRate: Float,
//                                       currentTime: Float
//                                       ) {
//
//  lazy val resolution: Float = buffer.size
//
//  private def sample(t: Float, f: Float): Float = {
//    val ind: Float = t * f * resolution
//    val a = ind.floor.toInt
//    val b = a + 1
//    val v1 = buffer(a % buffer.length)
//    val v2 = buffer(b % buffer.length)
//    Math.sqrt(v1 * v1 * (b - ind) * (b - ind) + v2 * v2 * (ind - a) * (ind - a)).toFloat * (if (v1 < 0) -1 else 1)
//    //    buffer(a % buffer.length) * (b - ind) + buffer(b % buffer.length) * (ind - a)
//    //    Sine(1f).at(t * f)
//  }
//
//  def getSamples(tone: ComplexTone, samples: Array[Float]): BufferedToneSynthesiser = {
//    def timeOfStep(i: Int): Float = currentTime + i / sampleRate
//    def componentHz(cmp: ComplexTone.Component): Float = tone.hz * cmp.hzRatio
//
//    // for each sample i...
//    0 until samples.size foreach {
//      i => {
//        // set sample i to sum over components of amplitude-adjusted samples
//        samples(i) = (0f /: tone.components)(
//          (sum: Float, cmp: ComplexTone.Component) =>
//            sum + cmp.ampRatio * sample(timeOfStep(i), componentHz(cmp))
//        )
//      }
//    }
//
//    new BufferedToneSynthesiser(buffer, sampleRate, timeOfStep(samples.size))
//  }
//}
//
//object BufferedToneSynthesiser {
//
//  def apply(resolution: Int,
//            sampleRate: Float = 44100,
//            currentTime: Float = 0
//             ): BufferedToneSynthesiser = {
//
//    val buffer = new Array[Float](resolution)
//
//    val wave = Sine(1) // wave at 1hz
//
//    0 until resolution foreach {
//      i =>
//        buffer(i) = wave.at(i.toFloat / resolution)
//    }
//
//    new BufferedToneSynthesiser(buffer, sampleRate, currentTime)
//  }
//}
//
