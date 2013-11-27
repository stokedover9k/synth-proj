package synth.scales

import util.LivingSpec
import org.specs2.specification.Scope

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 11/27/13
 * Time: 1:47 PM
 * To change this template use File | Settings | File Templates.
 */

class RameauScFourthsAndFifthsLivingSpec extends LivingSpec {
  def livingSpecFilename: String = "src/test/resources/rameau_sheet_full2.csv"

  val dataSkipRows: Int = 19
  val dataSkipCols: Int = 3
  val dataRows: Int = 92
  val dataCols: Int = 28

  lazy val fundamental = livingSpecData(dataSkipRows + 5)(dataSkipCols).toFloat

  "Fifths and Fourths off of Rameau" should {

    trait Fixture extends Scope {
      lazy val builder = ScaleBuilderRameau(fundamental)

      def to2Decimals(f: Float): String = "%.2f".format(f)

      def checkScale(builder: ScaleBuilderRameau, row: Int, offset: Int) = {
        val sc = builder.build
        val hzs = sc.intervals map (_.hz) map Math.round
        val ratios = sc.intervals map (_.hzFactor.toFloat) map to2Decimals
        mustEqualRow(hzs, row, offset, 8)
        mustEqualRow(ratios, row + 1, offset, 8)
      }

      def nFifthsUp(n: Int, builder: ScaleBuilderRameau = builder): ScaleBuilderRameau =
        if (n == 0) builder
        else nFifthsUp(n - 1, builder.fifthUpBuilder)

      def nFourthsUp(n: Int, builder: ScaleBuilderRameau = builder): ScaleBuilderRameau =
        if (n == 0) builder
        else nFourthsUp(n - 1, builder.fourthUpBuilder)
    }

    "compute baseline correctly" in new Fixture {
      checkScale(nFifthsUp(0), 5, 0)
    }

    "compute 1 fifth(s) up" in new Fixture {
      checkScale(nFifthsUp(1), 13, 4)
    }

    "compute 2 fifth(s) up" in new Fixture {
      checkScale(nFifthsUp(2), 20, 8)
    }

    "compute 3 fifth(s) up" in new Fixture {
      checkScale(nFifthsUp(3), 28, 12)
    }

    "compute 4 fifth(s) up" in new Fixture {
      checkScale(nFifthsUp(4), 36, 16)
    }

    "compute 5 fifth(s) up" in new Fixture {
      checkScale(nFifthsUp(5), 44, 20)
    }

    "compute 1 fourth(s) up" in new Fixture {
      checkScale(nFourthsUp(1), 45, 3)
    }

    "compute 2 fourth(s) up" in new Fixture {
      checkScale(nFourthsUp(2), 54, 6)
    }

    "compute 3 fourth(s) up" in new Fixture {
      checkScale(nFourthsUp(3), 63, 9)
    }

    "compute 4 fourth(s) up" in new Fixture {
      checkScale(nFourthsUp(4), 72, 12)
    }

    "compute 5 fourth(s) up" in new Fixture {
      checkScale(nFourthsUp(5), 81, 15)
    }

  }
}
