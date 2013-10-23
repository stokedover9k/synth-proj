package synth.scales

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 10/22/13
 * Time: 12:17 AM
 * To change this template use File | Settings | File Templates.
 */

import util.LivingSpec
import org.specs2.specification.Scope
import synth.SeriesHarmonic
import synth.Series.Interval
import scala.collection.immutable


class CentsHarmonicLivingSpec extends LivingSpec {
  def livingSpecFilename: String = "src/test/resources/DegreeNaming.csv"

  val dataSkipRows: Int = 3
  val dataSkipCols: Int = 9
  val dataRows: Int = 32
  val dataCols: Int = 9

  lazy val fundamental: Float = livingSpecData(dataSkipRows)(dataSkipCols + 1).toFloat

  "Harmonic Series" should {

    trait Fixture extends Scope {
      lazy val scale = ScaleBuilderHarmonicLong(fundamental, dataRows).build()

      lazy val allNotes = 0 until scale.size map (scale(_))

    }

    "compute correct frequencies" in new Fixture {
      mustEqualCol(allNotes map (_.hz) map Math.round, 1)
    }

    "compute correct cents values" in new Fixture {
      // The spec claims that we get 1200 cents on a 528 hz interval (degree 2) which is
      // inconsistent with the 0 cents on the first 528 hz interval (degree 1), so I adjust
      // this for consistency
      livingSpecData(dataSkipRows + 1)(dataSkipCols + 2) = "0"
      val cents = 0 until scale.size map (scale(_).cents) map Math.round
      mustEqualCol(cents, 2)
    }

    "compute closest natural degrees" in new Fixture {
      // The spec claims that we get closest to the 8th degree (octave) of the Pythagorean
      // scale on degree 2 (528 hz). I correct this to degree 1.
      livingSpecData(dataSkipRows + 1)(dataSkipCols) = "1"
      val closest = scale.allTypes map (_.toString) map (s => s.substring(s.size-1))
      mustEqualColUnlessEmpty(closest, 0)
    }

    "compute correct closest note names" in new Fixture {
      /*
       * In this column the spec is very inconsistent all over the place, so we skip this test.
       */
//      val names = 0 until scale.size map scale.getName
//      mustEqualCol(names, 6)
    }
  }
}
