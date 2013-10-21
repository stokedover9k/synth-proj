package synth.scales

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 10/20/13
 * Time: 9:36 PM
 * To change this template use File | Settings | File Templates.
 */

import util.LivingSpec
import org.specs2.specification.Scope
import synth.SeriesEvenTemp

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 10/20/13
 * Time: 2:37 PM
 * To change this template use File | Settings | File Templates.
 */

class DodecophonicPythagScLivingSpec extends LivingSpec {

  def livingSpecFilename: String = "src/test/resources/pythag_sheet_phase5.csv"

  val dataSkipRows: Int = 21
  val dataSkipCols: Int = 2
  val dataRows: Int = 15
  val dataCols: Int = 13

  lazy val fundamental: Float = livingSpecData(dataSkipRows + 11)(dataSkipCols).toFloat

  "Even Temperment Scale" should {

    trait Fixture extends Scope {

      /*
       * fill in the data values at the root note (omitted by the spec)
       */
      val fundCol: Int = 2
      livingSpecData(25)(fundCol) = "1" // factor
      livingSpecData(26)(fundCol) = fundamental.toString // interval frequency
      livingSpecData(27)(fundCol) = "0" // unadjusted octave
      livingSpecData(28)(fundCol) = "1" // octave adjustment
      livingSpecData(31)(fundCol) = "1"
      // adjusted factor

      lazy val series = new SeriesEvenTemp(fundamental)

      lazy val scale = ScaleBuilderDodecophonicFull(fundamental).build()

      lazy val allNotes = 0 to 12 map (scale(_))

      def to2DecimalPlaces(v: Float): Float = "%.2f".format(v).toFloat

      def to2DecimalPlaces(v: String): Float = to2DecimalPlaces(v.toFloat)
    }

    "have sorted the in the right order" in new Fixture {
      val degrees = allNotes map (_.degree)
      mustEqualRow(degrees, 0)
    }

    "compute correct unadjusted frequency ratios to the fundamental" in new Fixture {
      val ratios = allNotes map {
        _.generatingExpression.toFloat
      }
      mustEqualMappedRow(_.toFloat)(ratios, 4)
    }

    "compute correct unadjusted frequencies" in new Fixture {
      val hzs = allNotes map {
        _.hzUnscaled
      }
      mustEqualMappedRow(_.toFloat)(hzs, 5)
    }

    "compute correct adjusted ratios (within the octave)" in new Fixture {
      val ratios = allNotes map {
        _.hzFactor.toFloat
      }
      mustEqualMappedRow(_.toFloat)(ratios, 10)
    }

    "compute correct frequencies within the octave" in new Fixture {
      val fs = allNotes map {
        _.hz
      } map to2DecimalPlaces
      mustEqualMappedRow(to2DecimalPlaces)(fs, 11)
    }

    "compute the right note names" in new Fixture {
      val noteNames = 0 until scale.size map (scale.getName(_))
      mustEqualRow(noteNames, 13)
    }
  }
}
