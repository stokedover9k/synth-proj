package synth

import org.specs2.specification.Scope
import util.LivingSpec

/**
  * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 9/22/13
 * Time: 12:50 AM
 * To change this template use File | Settings | File Templates.
 */

class PythagoreanSeriesLivingSpec extends LivingSpec {

  def livingSpecFilename: String = "src/test/resources/pythag_sheet_phase2.csv"

  val dataSkipRows: Int = 5
  val dataSkipCols: Int = 3
  val dataRows: Int = 10
  val dataCols: Int = 14

  lazy val fundamental: Float = livingSpecData(dataSkipRows + dataRows - 1)(dataSkipCols + 1).toFloat


  "Pythagorean Series" should {

    trait Fixture extends Scope {

      /*
       * Fill in the values for the fundamental (omitted by spec) for conformity.
       */
      val fundCol: Int = 4
      livingSpecData (7)(fundCol) = "1"
      livingSpecData (8)(fundCol) = "1"
      livingSpecData (9)(fundCol) = "1"
      livingSpecData(10)(fundCol) = fundamental.toString
      livingSpecData(11)(fundCol) = "0"
      livingSpecData(12)(fundCol) = "1"
      livingSpecData(13)(fundCol) = "1"

      lazy val series = new SeriesPythagorean(fundamental)

      lazy val allNotes = 1 to dataCols map { x => series(x - 2) }

    }

    "compute correct set of indices" in new Fixture {
      val indices = allNotes map { _.degree }
      mustEqualRow(indices, 0)
    }

    "compute correct unadjusted frequency ratios to the fundamental" in new Fixture {
      val ratios = allNotes map { _.generatingExpression.toFloat }
      mustEqualMappedRow(_.toFloat)(ratios, 4)
    }

    "compute correct unadjusted frequencies" in new Fixture {
      val hzs = allNotes map { _.hzUnscaled }
      mustEqualMappedRow(_.toFloat)(hzs, 5)
    }

    "compute correct octaves" in new Fixture {
      val octaves = allNotes map { _.fallsInOctave }
      mustEqualRow(octaves, 6)
    }

    "compute correct octave adjustment" in new Fixture {
      val adjustments = allNotes map { _.octaveAdjustment.toFloat }
      mustEqualMappedRow(_.toFloat)(adjustments, 7)
    }

    "compute correct adjusted ratios (within the octave)" in new Fixture {
      val ratios = allNotes map { _.hzFactor.toFloat }
      mustEqualMappedRow(_.toFloat)(ratios, 8)
    }

    "compute correct frequencies within the octave" in new Fixture {
      val fs = allNotes map { _.hz } map { f => Math.round(f * 100) / 100f }
      mustEqualMappedRow(_.toFloat)(fs, 9)
    }
  }
}
