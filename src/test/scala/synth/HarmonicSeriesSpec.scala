package synth

import org.specs2.mutable._
import org.specs2.specification.Scope
import java.io.FileReader
import au.com.bytecode.opencsv.CSVReader



class HarmonicSeriesSpec extends Specification {

  "Harmonic Series" should {

    trait Fixture extends Scope {
      val livingSpecFileName = "src/test/resources/harmonic_series_sheet_phase1.csv"
      val lineStart = 3
      val colStart = 2
      val numLines = 33
      val numCols = 9
      lazy val fundamental = data(lineStart - 1)(colStart + numCols - 2).toFloat

      lazy val series = HarmonicSeries(fundamental)

      lazy val reader = new CSVReader(new FileReader(livingSpecFileName))

      def allNotes = 1 to numLines map { series(_) }

      lazy val data: Vector[Array[String]] = {
        var dt = Vector[Array[String]]()
        var row: Array[String] = null
        while( {row = reader.readNext(); row} != null ) dt = dt :+ row
        dt
      }

      def checkColumn[T]( col: Seq[T], colNum: Int ): Unit = {
        val dt = data.drop(lineStart-1).map { row => row(colNum - 1) }
        compareCols(col map { _.toString }, dt)
      }

      def checkFloatColumn( col: Seq[Float], colNum: Int ): Unit = {
        val dt = data.drop(lineStart-1).map { row => row(colNum -1).toFloat }
        compareCols( col, dt )
      }

      def compareCols[T]( col1: Seq[T], col2: Seq[T] ): Unit =
        col1 zip col2 foreach { x => x._1 must_== x._2 }

      def asFraction(fraction: (Int, Int)): String = fraction match { case (x, y) => "\"" + x +"/" + y + '"' }
    }

    "compute correct set of indices (col 2)" in new Fixture {
      val indices = allNotes map { x => x.degree }
      checkColumn(indices, 2)
    }

    "compute correct frequency ratios to fundamental (col 3)" in new Fixture {
      val ratioToFundamental = allNotes map { _.ratioToFundamental } map asFraction
      checkColumn(ratioToFundamental, 3)
    }

    "compute correct frequencies (col 4)" in new Fixture {
      val frequencies = allNotes map { _.simpleFrequency } map { f => if(f == f.toInt) f.toInt.toString else f.toString }
      checkColumn(frequencies, 4)
    }

    "compute correct octaves (col 5)" in new Fixture {
      val octaves = allNotes map { _.octave }
      checkColumn(octaves, 5)
    }

    "compute correct octave adjustment factors (col 6)" in new Fixture {
      val adj = allNotes map { _.octaveAdjustmentFactor }
      checkColumn(adj, 6)
    }

    "compute correct adjusted frequency ratios to fundamental (col 7)" in new Fixture {
      val adjFactors = allNotes map { _.ratioToFundamentalAdjusted } map asFraction
      checkColumn(adjFactors, 7)
    }

    "compute correct adjusted and reduced frequency ratios to the fundamental (col 8)" in new Fixture {
      val adjReduced = allNotes map { _.ratioToFundamentalAdjustedReduced } map asFraction
      checkColumn(adjReduced, 8)
    }

    "compute correct adjusted and reduced frequency ratios to the fundamental in decimal format (col 9)" in new Fixture {
      val adjReduced = allNotes map { _.ratioToFundamentalAdjustedReducedDecimal }
      checkFloatColumn(adjReduced, 9)
    }

    "compute correct frequencies within the octave (col 10)" in new Fixture {
      val frequencies = allNotes map { _.frequency } map { f => Math.round(f * 100) / 100f }
      checkFloatColumn(frequencies, 10)
    }
  }
}
