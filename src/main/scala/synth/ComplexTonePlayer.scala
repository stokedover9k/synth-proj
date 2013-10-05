package synth

import synth.ComplexTone.Component

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 10/4/13
 * Time: 10:27 PM
 * To change this template use File | Settings | File Templates.
 */

object ComplexTonePlayer {

  def main(args: Array[String]): Unit = {

    val line = Main.getLine

    val components =
      Seq(
        Component(1f, 1f),
        Component(2f, 0.7f),
        Component(3f, 0.4f),
        Component(5f, 0.3f),
        Component(6f, 0.15f),
        Component(7f, 0.1f)
      )

    var synthesiser = ToneSynthesiser(2048)

    val perSec = 5
    val samples = new Array[Float](synthesiser.sampleRate.toInt / perSec)

    def playForASecond(f: Float) = {
      def clamp: (Float => Float) = f => Math.max(-1f, Math.min(1f, f))
      val tone = ComplexTone(f, components)
      0 until perSec foreach {
        i => {
          synthesiser = synthesiser.getSamples(tone, samples)
          line.write(samples.map(y => (clamp(y) * 0.5 * 64).toByte), 0, samples.size)
        }
      }
    }

    object ScaleBuilder extends Series2Scale7[HarmonicSeries] with HarmonicSeries.Extracts7Notes
    val scale = ScaleBuilder buildScale( HarmonicSeries(528f) )

    0 until scale.size map {
      scale(_)
    } foreach {
      interval => playForASecond(interval.hz)
    }

    line.drain()
    line.stop()
    line.close()
  }

}
