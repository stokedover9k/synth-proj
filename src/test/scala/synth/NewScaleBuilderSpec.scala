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

  trait Fixture extends Scope {

    val fundamental = 528f

    lazy val harmonicSeries = HarmonicSeries(fundamental)
    lazy val pythagoreanSeries = PythagoreanSeries(fundamental)

    def iseqExtractor(s: NoteSeries): ExtractsUniqueIntervals = IntervalSequenceBuilder(s)

    lazy val harmonicScale = NewScale(iseqExtractor(harmonicSeries).extract(7).toArray[NoteSeries.Interval].sortBy(_.hz))
    lazy val pythagoreanScale = NewScale(iseqExtractor(pythagoreanSeries).extract(7).toArray[NoteSeries.Interval].sortBy(_.hz))
  }

  "Note extractor" should {

    "extract the right number of intervals from pythagorean and harmonic series" in new Fixture {
      iseqExtractor(harmonicSeries).extract(12).size mustEqual(12)
      iseqExtractor(pythagoreanSeries).extract(12).size mustEqual(12)
    }

    "extract unique intervals from harmonic series" in new Fixture {
      val extr = iseqExtractor(harmonicSeries)
      val intervals = extr.extract(12).toArray[NoteSeries.Interval].sortBy(_.hz)
      (intervals, intervals.tail).zipped.foreach {
        (a,b) => a.hz != b.hz
      }
    }

    "extract unique intervals from pythagorean series" in new Fixture {
      val extr = iseqExtractor(pythagoreanSeries)
      val intervals = extr.extract(12).toArray[NoteSeries.Interval].sortBy(_.hz)
      (intervals, intervals.tail).zipped.foreach {
        (a,b) => a.hz != b.hz
      }
    }
  }

  "NewScale" should {

    "build modes on Pythagorean series correctly" in new Fixture {
      val off = 3
      val mode = pythagoreanScale.modeAt(off)

      (0 until off) foreach { i =>
        pythagoreanScale(i).hz * 2 must_== mode(mode.size - off + i).hz
      }
      (off until pythagoreanScale.size) foreach { i =>
        pythagoreanScale(i).hz must_== mode(i - off).hz
      }
    }

    "build modes on Harmonic series correctly" in new Fixture {
      val off = 3
      val mode = harmonicScale.modeAt(off)

      (0 until off) foreach { i =>
        harmonicScale(i).hz * 2 must_== mode(mode.size - off + i).hz
      }
      (off until harmonicScale.size) foreach { i =>
        harmonicScale(i).hz must_== mode(i - off).hz
      }
    }

    "give octave that matches the corresponding frequency in the mode" in new Fixture {
      val off = 3
      pythagoreanScale.modeAt(off)(pythagoreanScale.size - off).hz must_== pythagoreanScale.octave.hz
      harmonicScale.modeAt(off)(harmonicScale.size - off).hz must_== harmonicScale.octave.hz
    }
  }

}
