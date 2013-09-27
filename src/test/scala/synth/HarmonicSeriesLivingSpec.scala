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

      lazy val allNotes = 1 to dataRows map { series(_) }
    }

    "compute correct set of indices (col 2)" in new Fixture {
      val indices = allNotes map { _.degree }
      mustEqualCol(indices, 0)
    }

    "compute correct frequencies within the octave (col 10)" in new Fixture {
      val frequencies = allNotes map {
        _.hz
      } map {   // remap to two decimal places of precision
        f => Math.round(f * 100) / 100f
      }
      mustEqualMappedCol(_.toFloat)(frequencies, 8)
    }
  }
}
