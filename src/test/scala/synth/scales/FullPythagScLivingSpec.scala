package synth.scales

import org.specs2.specification.Scope
import util.LivingSpec

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 10/19/13
 * Time: 9:24 PM
 * To change this template use File | Settings | File Templates.
 */
class FullPythagScLivingSpec extends LivingSpec {

  def livingSpecFilename: String = "src/test/resources/pythag_sheet_phase4.csv"

  val dataSkipRows: Int = 18
  val dataSkipCols: Int = 2
  val dataRows: Int = 14
  val dataCols: Int = 14

  lazy val fundamental: Float = livingSpecData(dataSkipRows + dataRows - 3)(dataSkipCols).toFloat

    "Full Pythagorean Scale" should {

    trait Fixture extends Scope {

      /*
       * Fill in the values for the fundamental (omitted by spec) for conformity.
       */
      val fundCol: Int = 2
      livingSpecData(22)(fundCol) = "1" // factor
      livingSpecData(23)(fundCol) = fundamental.toString // interval frequency
      livingSpecData(24)(fundCol) = "0" // unadjusted octave
      livingSpecData(25)(fundCol) = "1" // octave adjustment
      livingSpecData(28)(fundCol) = "1" // adjusted factor

      livingSpecData(31)(8) = "F#" // switch flat and sharp
      livingSpecData(31)(9) = "Gb"

      lazy val scale = ScaleBuilderPythagFull(fundamental).build()

      lazy val allIntervals = 0 until dataCols map {
        scale(_)
      } match {  // the last note returned is in the second octave, so we drop it down to first to match the spec
        case ns => ns.take(ns.size - 1) :+ ns.drop(ns.size - 1).head.octaveDown
      }

      lazy val allNotes = 0 until dataCols map { scale.getName(_) }

    }

    "have sorted the in the right order" in new Fixture {
      val degrees = allIntervals map (_.degree)
      mustEqualRow(degrees, 0)
    }

    "compute correct unadjusted frequency ratios to the fundamental" in new Fixture {
      val ratios = allIntervals map {
        _.generatingExpression.toFloat
      }
      mustEqualMappedRow(_.toFloat)(ratios, 4)
    }

    "compute correct unadjusted frequencies" in new Fixture {
      val hzs = allIntervals map {
        _.hzUnscaled
      }
      mustEqualMappedRow(_.toFloat)(hzs, 5)
    }

    "compute correct octaves" in new Fixture {
      val octaves = allIntervals map {
        _.fallsInOctave
      }
      mustEqualRow(octaves, 6)
    }

    "compute correct octave adjustment" in new Fixture {
      val adjustments = allIntervals map {
        _.octaveAdjustment.toFloat
      }
      mustEqualMappedRow(_.toFloat)(adjustments, 7)
    }

    "compute correct adjusted ratios (within the octave)" in new Fixture {
      val ratios = allIntervals map {
        _.hzFactor.toFloat
      }
      mustEqualMappedRow(_.toFloat)(ratios, 10)
    }

    "compute correct frequencies within the octave" in new Fixture {
      val fs = allIntervals map {
        _.hz
      } map {
        f => Math.round(f * 100) / 100f
      }
      mustEqualMappedRow(_.toFloat)(fs, 11)
    }

    "compute the right note names" in new Fixture {
      mustEqualRow(allNotes, 13)
    }
  }
}
