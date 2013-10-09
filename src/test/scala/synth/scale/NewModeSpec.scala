package synth.scale

import org.specs2.mutable._
import org.specs2.specification.Scope
import synth.scales.{ModeScale, MyScale, ScaleInterface}
import synth.{NoteSeries, PythagoreanSeries}
import org.specs2.execute.Result

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 10/9/13
 * Time: 12:35 PM
 * To change this template use File | Settings | File Templates.
 */

class NewModeSpec extends Specification {

  trait Fixture extends Scope {
    lazy val hzs: IndexedSeq[Float] = IndexedSeq(528.00, 594.00, 668.25, 704.00, 792.00, 891.00, 1002.38) map (_.toFloat)
    lazy val notes: IndexedSeq[String] = IndexedSeq("C", "D", "E", "F", "G", "A", "B")

    lazy val hzs2: IndexedSeq[Float] = IndexedSeq(668.25, 704.00, 792.00, 891.00, 1002.38, 1056.00, 1188.00) map (_.toFloat)
    lazy val notes2: IndexedSeq[String] = IndexedSeq("E", "F", "G", "A", "B", "C", "D")

    lazy val series = PythagoreanSeries(528f)
    lazy val intervals = -1 to 5 map { series(_) } sortBy(_.hz)

    lazy val buildScale : (IndexedSeq[NoteSeries.Interval], IndexedSeq[String]) => ModeScale =
      (is, ns) => new ModeScale(is, ns)

    lazy val scale: ModeScale = buildScale(intervals, notes)

    def to2Decimals(f: Float) = Math.round(f * 100) / 100f
  }

  "New Mode Scales (without octave on the end)" should {

    "produce expected modes" in new Fixture {

      val myhzs = scale.mode(2).intervals map (_.hz) map to2Decimals
      (myhzs, hzs2).zipped.forall { (a,b) => a must_== b }
    }

  }
}
