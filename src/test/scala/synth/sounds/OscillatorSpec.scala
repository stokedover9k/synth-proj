package synth.sounds

import org.specs2.mutable._
import org.specs2.specification.Scope

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 9/13/13
 * Time: 10:25 PM
 * To change this template use File | Settings | File Templates.
 */
class OscillatorSpec extends Specification {

  "Oscillator" should {

    trait Fixture extends Scope {
      val sr = 8   // buffer Rate
      val f = 2    // frequency
      val samples = new Array[Float](sr)
//      var o: Oscillator = new WaveOscillator(sr)
      var o: Oscillator = BufferedWaveOscillator(512, Sine, sr, 0)

      def checkSamples( expected: Seq[Double] ) {
        samples zip expected foreach { case (x, y) => x.toDouble should be ~(y +/- 0.001) }
      }
    }

    // sine wave looks like this ^~_~^~_~^~_~

    "samples are extracted with right values" in new Fixture {
      o = o.getSamples(f, samples, 4, 0)     // ^~_~~~~~
      checkSamples( Seq[Double](0, 1, 0, -1, 0, 0, 0, 0) )
    }

    "samples are extracted with right values at offset" in new Fixture {
      o = o.getSamples(f, samples, 4, 4)     // ~~~~~^~_
      checkSamples( Seq[Double](0, 0, 0, 0, 0, 1, 0, -1))
    }

    "consequent sampling proceeds from previous position on the sound wave" in new Fixture {
      o = o.getSamples(f, samples, 2, 0)  // ^~~~~~~~~~
      o = o.getSamples(f, samples, 2, 6)  // ^~~~~~~~~_
      checkSamples( Seq[Double](0, 1, 0, 0, 0, 0, 0, -1) )
    }

    "samples of different size lead to equivalent values" in new Fixture {
      val other = new Array[Float](samples.size * 2)
      o.getSamples(f, samples)
      o.getSamples(f, other)
      samples zip other foreach { case (x, y) => x must_== y }
    }

  }
}
