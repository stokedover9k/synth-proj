package synth

import org.specs2.mutable._
import org.specs2.specification.Scope

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 10/7/13
 * Time: 3:17 PM
 * To change this template use File | Settings | File Templates.
 */

class NewScaleBuilderSpec extends Specification {


  "New Scales" should {

    abstract class Fixture( seriesCreator: Float => NoteSeries ) extends Scope {

      val fundamental = 528f

      lazy val series: NoteSeries = seriesCreator(fundamental)

      lazy val intervals7:  IndexedSeq[NoteSeries.Interval] = IntervalSequenceBuilder(series).extract(7).toIndexedSeq.sortBy(_.hz)
      lazy val intervals12: IndexedSeq[NoteSeries.Interval] = IntervalSequenceBuilder(series).extract(12).toIndexedSeq.sortBy(_.hz)

      lazy val names7  = Notes.startingWith(Notes.C, Notes.tones)
      lazy val names12 = Notes.startingWith(Notes.C, Notes.tonesWithSharps)

      lazy val scale7  = ScaleWithModes( intervals7,  names7, 0)
      lazy val scale12 = ScaleWithModes(intervals12, names12, 0)
    }

    class FixtureH extends Fixture( HarmonicSeries.apply )
    class FixtureP extends Fixture( PythagoreanSeries.apply )

    "compile" in {
      1 must_==(1)
    }

    "get created with Pythagorean Series" in new FixtureP { scale7 must_!= null }
    "get created with Harmonic Series" in new FixtureH { scale7 must_!= null }

    "get created with proper frequencies" in new FixtureP {
      scale12(2).hz must_== 594f
      scale7(1).hz  must_== 594f
    }

    "give modes whose wrapped frequencies are correct" in new FixtureP {
      scale7.modeAt(2)(5).hz must_== scale7(0).octaveUp.hz
    }

    "give correct frequency for the octave" in new FixtureP {
      scale7(0).hz * 2 must_== scale7(7).hz
    }

    "7 tone scale should throw when queried for A#" in new FixtureP {
      try {
        scale7.getInterval(Notes.As)
        failure("7 scale did not throw")
      } catch {
        case _: Throwable => success
      }
    }

    "12 tone scale should succeed (not throw) when queried for at least one of A# or Bb" in new FixtureP {
      try {
        scale12.getInterval(Notes.As)
        success
      } catch {
        case _: Throwable => try {
          scale12.getInterval(Notes.Bb)
          success
        } catch {
          case _: Throwable => failure("12 tone did not throw")
        }
      }
    }

    "7 tone scale and its mode should return the right interval for G" in new FixtureP {
      scale7.getInterval(Notes.G).hz must_== 792f
      scale7.modeAt(3).getInterval(Notes.G).hz must_== 792f
    }
  }
}
