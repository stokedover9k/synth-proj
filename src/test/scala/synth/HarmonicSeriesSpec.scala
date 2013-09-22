package synth

import org.specs2.specification.Scope
import util.LivingSpec

class HarmonicSeriesLivingSpec extends LivingSpec {

  def livingSpecFilename: String = "src/test/resources/harmonic_series_sheet_phase1.csv"

  val dataStartRow: Int = 2
  val dataStartCol: Int = 1
  val dataRows: Int = 33
  val dataCols: Int = 9

  lazy val fundamental = livingSpecData(dataStartRow)(dataStartCol + dataCols - 1).toFloat

  "Harmonic Series" should {

    trait Fixture extends Scope {

      lazy val series = HarmonicSeries(fundamental)

      lazy val allNotes = 1 to dataRows map { series(_) }
    }

    "compute correct set of indices (col 2)" in new Fixture {
      val indices = allNotes map { x => x.degree }
      mustEqualCol(indices, 1)
    }

    "compute correct frequency ratios to fundamental (col 3)" in new Fixture {
      val ratioToFundamental = allNotes map { _.hzSimpleRatio }
      mustEqualCol(quotedSeq(ratioToFundamental), 2)
    }

    "compute correct frequencies (col 4)" in new Fixture {
      val frequencies = allNotes map {
        _.hzSimple
      } map {      // remap so that decimal places show up only if not .0
        f => if (f == f.toInt) f.toInt.toString else f.toString
      }
      mustEqualCol(frequencies, 3)
    }

    "compute correct octaves (col 5)" in new Fixture {
      val octaves = allNotes map { _.octave }
      mustEqualCol(octaves, 4)
    }

    "compute correct octave adjustment factors (col 6)" in new Fixture {
      val adj = allNotes map { _.adjustFactor }
      mustEqualCol(adj, 5)
    }

    "compute correct adjusted frequency ratios to fundamental (col 7)" in new Fixture {
      val adjFactors = allNotes map { _.hzAdjustedRatio }
      mustEqualCol(quotedSeq(adjFactors), 6)
    }

    "compute correct adjusted and reduced frequency ratios to the fundamental (col 8)" in new Fixture {
      val adjReduced = allNotes map { _.hzReducedRatio }
     mustEqualCol(quotedSeq(adjReduced), 7)
    }

    "compute correct adjusted and reduced frequency ratios to the fundamental in decimal format (col 9)" in new Fixture {
      val adjReduced = allNotes map { _.hzDecimalRatio }
      mustEqualMappedCol(_.toFloat)(adjReduced, 8)
    }

    "compute correct frequencies within the octave (col 10)" in new Fixture {
      val frequencies = allNotes map {
        _.hz
      } map {   // remap to two decimal places of precision
        f => Math.round(f * 100) / 100f
      }
      mustEqualMappedCol(_.toFloat)(frequencies, 9)
    }
  }
}
