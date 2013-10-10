package synth.scale

import org.specs2.mutable._
import org.specs2.specification.Scope
import synth.scales.MyScale
import synth.scales.ScaleInterface
import synth.{NoteSeries, PythagoreanSeries}
import org.specs2.execute.Result

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 10/8/13
 * Time: 10:43 PM
 * To change this template use File | Settings | File Templates.
 */

class NewScaleSpec extends Specification {

  trait Fixture extends Scope {

    lazy val hzs: IndexedSeq[Float] = IndexedSeq(528.00, 594.00, 668.25, 704.00, 792.00, 891.00, 1002.38) map (_.toFloat)
    lazy val notes: IndexedSeq[Note] = IndexedSeq("C", "D", "E", "F", "G", "A", "B").map (s => Note.makeNote(s, 0).get)

    lazy val series = PythagoreanSeries(528f)
    lazy val intervals = -1 to 5 map { series(_) } sortBy(_.hz)

    lazy val buildScale : (IndexedSeq[NoteSeries.Interval], IndexedSeq[Note]) => ScaleInterface = MyScale.apply
    lazy val scale: ScaleInterface = buildScale(intervals, notes)

    def to2Decimals(f: Float) = Math.round(f * 100) / 100f
  }

  "New Scale" should {

    "get constructed from a sequence of frequencies" in new Fixture {
      failure("not implemented")
    }
    
    "have as many intervals as expected" in new Fixture {
      scale.intervals.size must_== hzs.size
    }

    "have as many named notes as expected" in new Fixture {
      scale.notes.size must_== hzs.size
    }

    "have the right notes in sequence" in new Fixture {
      (scale.notes, notes).zipped.forall { (a, b) => a must_== b }
    }

    "have the right frequencies in sequence" in new Fixture {
      val myhzs = scale.intervals map {_.hz} map to2Decimals
      (myhzs, hzs).zipped.forall( (a, b) => a must_== b )
    }
  }
}
