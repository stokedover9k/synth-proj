package synth

import org.specs2.specification.Scope
import org.specs2.mutable.Specification
import org.specs2.matcher._
import scala.util.Sorting


class ScaleSpec extends Specification {

  "Scales" should {

    trait Fixture extends Scope {
      lazy val freqs = Array(528.00, 594.00, 668.25, 704.00, 792.00, 891.00, 1002.38).map(_.toFloat)
      lazy val scale = Scale7(freqs)

      case class BeMode(scale: Scale, offset: Int) extends Matcher[Scale] {
        def apply[S <: Scale](s: Expectable[S]) = {

          // 1. size of scales must equal
          // 2. first (size-offset) elements of mode must equal elements of scale starting at offset
          // 3. the rest of elements of mode must be twice the first elements (0 to offset) of scale
          def equalSize = s.value.size == scale.size
          def firstNotesMatch = (0 until scale.size - offset).forall { i => s.value(i) == scale(offset + i) }
          def lastNotesMatch = (0 until offset).forall { i => s.value(scale.size - offset + i) == 2 * scale(i) }

          result( equalSize && firstNotesMatch && lastNotesMatch,
            s.description + " is mode %d of\n %s".format(offset, scale),      // message if succeeds
            s.description + " is not mode %d of\n %s".format(offset, scale),  // message if fails
            s
          )
        }
      }
    }

    "have 8 elements" in new Fixture {
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

      val hzs: Array[Float] = (-1 to 5 map (ps(_).hz)).toArray
      Sorting.quickSort(hzs)
      val s2 = Scale7(hzs)

      s1 must BeMode( s2, 0 )
    }
  }
}
