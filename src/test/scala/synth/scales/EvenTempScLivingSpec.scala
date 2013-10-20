package synth.scales

import util.LivingSpec
import org.specs2.specification.Scope
import synth.EvenTempSeries

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 10/20/13
 * Time: 2:37 PM
 * To change this template use File | Settings | File Templates.
 */


class EvenTempScLivingSpec extends LivingSpec {
  def livingSpecFilename: String = "src/test/resources/even_temperment_sheet_phase4.csv"

  val dataSkipRows: Int = 3
  val dataSkipCols: Int = 8
  val dataRows: Int = 13
  val dataCols: Int = 7

  lazy val fundamental: Float = livingSpecData(dataSkipRows)(dataSkipCols).toFloat

  "Even Temperment Scale" should {

    trait Fixture extends Scope {

      lazy val series = new EvenTempSeries(fundamental)

      lazy val scale = ScBuilderEvenTempFull(fundamental).build()

      lazy val allNotes = 0 to 12 map ( n => "%.2f".format( scale(n).hz ).toFloat )
    }

    "compute correct frequencies" in new Fixture {
      mustEqualMappedCol(_.toFloat)(allNotes, 0)
    }
  }
}
