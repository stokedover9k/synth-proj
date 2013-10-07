package synth

import util.LivingSpec
import org.specs2.specification.Scope
import org.specs2.matcher._

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 10/3/13
 * Time: 1:40 AM
 * To change this template use File | Settings | File Templates.
 */

class Scale7LivingSpec extends LivingSpec {
  def livingSpecFilename: String = "src/test/resources/pythag_sheet_phase3.csv"

  val dataSkipRows: Int = 13
  val dataSkipCols: Int = 4
  val dataRows: Int = 8
  val dataCols: Int = 15

  trait Fixture extends Scope {

    lazy val fundamental: Float = livingSpecData(dataSkipRows)(dataSkipCols).toFloat

    lazy val scaleBuilder = new Series2Scale7[PythagoreanSeries] with PythagoreanSeries.Extracts7Notes

    lazy val scale: Scale7 = scaleBuilder.buildScale7(new PythagoreanSeries(fundamental))

    case class matchExpectedAt(offset: Int) extends Matcher[Scale] {
      def apply[S <: Scale](s: Expectable[S]) = {

        def to2decimalPlaces: Float => Float =
          f => Math.round(f * 100).toFloat / 100

        def expectedVals = getDataRow(offset) drop offset take 8 map (_.toFloat)
        def actualVals = 0 to 7 map (s.value(_).hz) map to2decimalPlaces

        result(
          (expectedVals, actualVals).zipped.forall {        // Zip expected and actual values: (e1, a1), (e2, a2), ... (e_n, a_n)
            ea: (Float, Float) => { ea._1 == ea._2 }        // Check that e_i (ea._1) matches a_i (ea._2)
          },
          s.description + " is mode %d:\n %s".format(offset, expectedVals), // message if succeeds
          s.description + " is not mode %d:\n %s".format(offset, expectedVals), // message if fails
          s
        )
      }
    }
  }

  "7 tone scale constructed from Pythagorean series" should {

    "produce correct Ionian mode" in new Fixture {
      scale.Ionian must matchExpectedAt(0)
    }

    "produce correct Dorian mode" in new Fixture {
      scale.Dorian must matchExpectedAt(1)
    }

    "produce correct Phrygian mode" in new Fixture {
      scale.Phrygian must matchExpectedAt(2)
    }

    "produce correct Lydian mode" in new Fixture {
      scale.Lydian must matchExpectedAt(3)
    }

    "produce correct Mixolydian mode" in new Fixture {
      scale.Mixolydian must matchExpectedAt(4)
    }

    "produce correct Aeolian mode" in new Fixture {
      scale.Aeolian must matchExpectedAt(5)
    }

    "produce correct Locrian mode" in new Fixture {
      scale.Locrian must matchExpectedAt(6)
    }

    "produce correct Ionian Octave mode" in new Fixture {
      scale.IonianOctave must matchExpectedAt(7)
    }

  }
}
