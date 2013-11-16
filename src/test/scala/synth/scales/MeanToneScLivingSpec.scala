package synth.scales

import org.specs2.specification.Scope
import util.LivingSpec

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 11/16/13
 * Time: 5:41 PM
 * To change this template use File | Settings | File Templates.
 */

class MeanToneScLivingSpec extends LivingSpec {

  def livingSpecFilename: String = "src/test/resources/meantone_phase7.csv"

  val dataSkipRows: Int = 0
  val dataSkipCols: Int = 2
  val dataRows: Int = 36
  val dataCols: Int = 17

  lazy val fundamental: Float = livingSpecData(dataSkipRows + 15)(dataSkipCols + 6).toFloat

  "Mean Tone scale" should {

    trait Fixture extends Scope {
      lazy val scale = ScaleBuilderMeanTone(fundamental).build
      lazy val allIntervals = scale.intervals
    }

    "compute correct sorted frequency ratios" in new Fixture {
      def round(f: Float) = "%.5f".format(f).toFloat

      mustEqualMappedRow(f => round(f.toFloat))(allIntervals map (_.hzFactor.toFloat) map round, 33)
    }

    "compute correct note names" in new Fixture {
      mustEqualRow(scale.allNames, 34)
    }

    "compute correct interval types" in new Fixture {
      if (livingSpecData(dataSkipRows + 35)(dataSkipCols + 3) == "2")  // the spec claims "2" instead of M2
        livingSpecData(dataSkipRows + 35)(dataSkipCols + 3) = "M2"

      mustEqualRowUnlessEmpty(scale.allTypes, 35)
    }
  }
}
