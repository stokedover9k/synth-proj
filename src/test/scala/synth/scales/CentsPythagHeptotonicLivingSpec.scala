package synth.scales

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 10/21/13
 * Time: 2:57 PM
 * To change this template use File | Settings | File Templates.
 */

import util.LivingSpec
import org.specs2.specification.Scope


class CentsPythagHeptotonicLivingSpec extends LivingSpec {
  def livingSpecFilename: String = "src/test/resources/DegreeNaming.csv"

  val dataSkipRows: Int = 27
  val dataSkipCols: Int = 1
  val dataRows: Int = 8
  val dataCols: Int = 7

    lazy val fundamental: Float = livingSpecData(dataSkipRows)(dataSkipCols+1).toFloat

  "Pythagorean Scale" should {

    trait Fixture extends Scope {

      lazy val scale = ScaleBuilderPythagHepto(fundamental).build()

      lazy val allNotes = 0 until scale.size map (scale(_))
    }

    "compute correct frequencies" in new Fixture {
      val hzs = allNotes map (_.hz) map Math.round
      mustEqualCol(hzs, 1)
    }

    "compute correct cents values" in new Fixture {
      val cents = allNotes map (_.cents) map Math.round
      mustEqualCol(cents, 2)
    }

    "compute correct note types" in new Fixture {
      val types = 0 until scale.size map scale.getType
      mustEqualCol(types, 4)
    }

    "compute correct note names" in new Fixture {
      val names = 0 until scale.size map scale.getName
      mustEqualMappedCol(_.trim)(names, 6)
    }
  }
}
