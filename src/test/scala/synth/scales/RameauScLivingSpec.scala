package synth.scales

import org.specs2.specification.Scope
import util.LivingSpec

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 11/26/13
 * Time: 3:11 PM
 * To change this template use File | Settings | File Templates.
 */

class RameauScLivingSpec extends LivingSpec {
  def livingSpecFilename: String = "src/test/resources/rameau_sheet_full.csv"

  val dataSkipRows: Int = 32
  val dataSkipCols: Int = 13
  val dataRows: Int = 8
  val dataCols: Int = 5

  lazy val fundamental: Float = 528f

  "Rameau Scale" should {

    trait Fixture extends Scope {
      lazy val scale = ScaleBuilderRameau(fundamental).build

      def to2Decimals(f: Float): Float = "%.2f".format(f).toFloat
    }

    "compute correct frequency ratios (decimal)" in new Fixture {
      val ratios = scale.intervals.map(_.hzFactor.toFloat) map to2Decimals
      mustEqualMappedCol(_.toFloat)(ratios, 0)
    }

    "compute correct frequency ratios (fraction)" in new Fixture {
      val ratios = scale.intervals.map(_.hzFactor) map {
        f => f.toString // to string
      } map {
        str => if (str.size == 1) str + "/1" else str // add denominator where fraction is a whole number
      } map {
        str => "\"" + str + "\"" // add quotes (silly spec)
      }
      mustEqualCol(ratios, 1)
    }

    "compute correct interval types" in new Fixture {
      livingSpecData(33)(15) = "M2" // spec has "whole tone"
      livingSpecData(39)(15) = "1" // spec has 8 for the octave
      mustEqualCol(scale.allTypes, 2)
    }

    "compute correct cents" in new Fixture {
      val cents = scale.intervals map (_.cents) map Math.round
      mustEqualCol(cents, 3)
    }

    "compute correct note names" in new Fixture {
      mustEqualCol(scale.allNames, 4)
    }
  }
}
