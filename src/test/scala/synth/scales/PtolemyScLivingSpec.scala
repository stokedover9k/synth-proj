package synth.scales

import org.specs2.specification.Scope
import util.LivingSpec
import util.expr.Fraction

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 10/31/13
 * Time: 12:17 AM
 * To change this template use File | Settings | File Templates.
 */

class PtolemyScLivingSpec extends LivingSpec {

  def livingSpecFilename: String = "src/test/resources/pythag_ptolemy_sheet_phase6.csv"

  val dataSkipRows: Int = 2
  val dataSkipCols: Int = 2
  val dataRows: Int = 40
  val dataCols: Int = 13

  lazy val fundamental: Float = livingSpecData(dataSkipRows + 11)(dataSkipCols).toFloat

  "Full Pythagorean Scale" should {

    trait Fixture extends Scope {

      lazy val scale = ScaleBuilderPtolemyChromatic(fundamental).build()

    }

    "compute correct frequency ratios" in new Fixture {

      val ratios = scale.intervals map (_.hzFactor) map {
        case fraction: Fraction => fraction
        case _ => sys.error("this is weird")
      }

      val numerators = ratios map (_.num.toString) splitAt (6) match {
        case (head, tail) => head ++ Seq("REMOVED") ++ tail
      }
      val denominators = ratios map (_.denom.toString) splitAt (6) match {
        case (head, tail) => head ++ Seq("") ++ tail
      }

      mustEqualRow(numerators, 38)
      mustEqualRow(denominators, 39)
    }
  }
}
