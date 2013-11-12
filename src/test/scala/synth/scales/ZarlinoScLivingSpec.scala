package synth.scales

import org.specs2.specification.Scope
import util.LivingSpec
import java.text.DecimalFormat

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 11/12/13
 * Time: 12:06 PM
 * To change this template use File | Settings | File Templates.
 */

class ZarlinoScLivingSpec extends LivingSpec {

  def livingSpecFilename: String = "src/test/resources/zarlino.csv"

  val dataSkipRows: Int = 0
  val dataSkipCols: Int = 2
  val dataRows: Int = 30
  val dataCols: Int = 8

  lazy val fundamental: Float = livingSpecData(dataSkipRows + 15)(dataSkipCols).toFloat

  "Zarlino Scale" should {

    trait Fixture extends Scope {

      lazy val scale = ScaleBuilderZarlino(fundamental).build()

    }

    "compute correct degrees" in new Fixture {
      val degrees = scale.intervals map (_.degree)
      mustEqualRow(degrees, 5)
    }

    "compute correct frequencies" in new Fixture {
      val formatter = new DecimalFormat("#.#")
      val hz = scale.intervals map (v => formatter.format(v.hz))
      println(hz)

      15 until 20 foreach {
        row =>
          mustEqualRowUnlessEmpty(hz, row)
      }
    }

    "compute correct note names" in new Fixture {
      val notes = scale.allNames
      mustEqualRow(notes, 22)
    }

    "compute correct interval types" in new Fixture {
      val types = scale.allTypes map (_.toString)
      mustEqualRowUnlessEmpty(types, 21)
    }
  }
}
