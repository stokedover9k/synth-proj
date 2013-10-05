package synth

import org.specs2.specification.Scope
import org.specs2.mutable.Specification
import org.specs2.matcher._
import synth.NoteSeries.Interval

class ScaleSpec extends Specification {

  "Scales" should {

    trait Fixture extends Scope {
      lazy val series = PythagoreanSeries(528)
      lazy val intervals = -1 to 5 map {i => series(i)} sortBy(_.hz)
      lazy val scale = Scale7(intervals)

      case class BeMode(scale: Scale, offset: Int) extends Matcher[Scale] {
        def apply[S <: Scale](s: Expectable[S]) = {

          // 1. size of scales must equal
          // 2. first (size-offset) elements of mode must equal elements of scale starting at offset
          // 3. the rest of elements of mode must be twice the first elements (0 to offset) of scale
          def equalSize = s.value.size == scale.size
          def firstNotesMatch = (0 until scale.size - offset).forall { i => s.value(i) == scale(offset + i) }
          def lastNotesMatch = (0 until offset).forall { i => s.value(scale.size - offset + i).hz == 2 * scale(i).hz }

          result( equalSize && firstNotesMatch && lastNotesMatch,
            s.description + " is mode %d of\n %s".format(offset, scale),      // message if succeeds
            s.description + " is not mode %d of\n %s".format(offset, scale),  // message if fails
            s
          )
        }
      }
    }

    "have 7 elements" in new Fixture {
      scale.size must_== 7
    }

    "create modes correctly" in new Fixture {
      0 until 7 forall { i =>
        scale.mode(i) must BeMode(scale, i)
      }
    }

    "correctly construct a 7 note scale from pythagorean series" in new Fixture {
      object PythagoreanToScale extends Series2Scale7[PythagoreanSeries] with PythagoreanSeries.Extracts7Notes

      val ps = new PythagoreanSeries(528)

      val s1 = PythagoreanToScale.buildScale(ps)

      val hzs: Array[Interval] = (-1 to 5 map (ps(_))).toArray[Interval]
      val s2 = Scale7(hzs.sortBy(_.hz))

      s1 must BeMode( s2, 0 )
    }
  }
}
