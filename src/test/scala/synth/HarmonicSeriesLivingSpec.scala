package synth

import org.specs2.specification.Scope
import util.LivingSpec

class HarmonicSeriesLivingSpec extends LivingSpec {

  def livingSpecFilename: String = "src/test/resources/harmonic_series_sheet_phase1.csv"

  val dataSkipRows: Int = 2
  val dataSkipCols: Int = 1
  val dataRows: Int = 33
  val dataCols: Int = 9

  lazy val fundamental = livingSpecData(dataSkipRows)(dataSkipCols + dataCols - 1).toFloat

  "Harmonic Series" should {

    trait Fixture extends Scope {

      lazy val series = new HarmonicSeries(fundamental)

      lazy val allNotes = 0 until dataRows map { series(_) }
    }

    "compute correct set of indices (col B)" in new Fixture {
      val indices = allNotes map { _.degree + 1 }  // +1 because series is 0-indexed, while spec is 1-indexed
      mustEqualCol(indices, 0)
    }

    "compute correct unadjusted frequencies (col D)" in new Fixture {
      val hzs = allNotes map { _.hzUnscaled }
      mustEqualMappedCol(_.toFloat)(hzs, 2)
    }

    "compute correct octave (col E)" in new Fixture {
      val octaves = allNotes map { _.octave + 1 }
      mustEqualCol(octaves, 3)
    }

    "compute correct octave adjustment (col F)" in new Fixture {
      val adjustments = allNotes map { _.octaveAdjustment.toFloat }
      mustEqualMappedCol(_.toFloat)(adjustments, 4)
    }

    "compute correct adjusted ratios (within the octave) (col I)" in new Fixture {
      val ratios = allNotes map { _.hzFactor.toFloat }
      mustEqualMappedCol(_.toFloat)(ratios, 7)
    }

    "compute correct frequencies within the octave (col J)" in new Fixture {
      val frequencies = allNotes map {
        _.hz
      } map {   // remap to two decimal places of precision
        f => Math.round(f * 100) / 100f
      }
      mustEqualMappedCol(_.toFloat)(frequencies, 8)
    }
  }
}
