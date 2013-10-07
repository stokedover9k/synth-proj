package synth

import util.LivingSpec
import org.specs2.specification.Scope

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 10/7/13
 * Time: 12:05 PM
 * To change this template use File | Settings | File Templates.
 */

class Scale14LivingSpec extends LivingSpec {
  def livingSpecFilename: String = "src/test/resources/pythag_sheet_phase4.csv"

  val dataSkipRows: Int = 18
  val dataSkipCols: Int = 2
  val dataRows: Int = 12
  val dataCols: Int = 14

  trait Fixture extends Scope {

    lazy val fundamental: Float = livingSpecData(dataSkipRows)(dataSkipCols + dataCols-1).toFloat

    lazy val scaleBuilder = new Series2Scale14[PythagoreanSeries] with PythagoreanSeries.Extracts14Notes

    lazy val scale: Scale14 = scaleBuilder.buildScale14(new PythagoreanSeries(fundamental))

    lazy val allNotes = 0 until 14 map (scale(_))
  }

  "14 tone scale constructed from Pythagorean series" should {

    "have sorted intervals so the degrees are in the right order" in new Fixture {
      val degrees = allNotes map { _.degree }
      mustEqualRow(degrees, 0)
    }
  }
}
