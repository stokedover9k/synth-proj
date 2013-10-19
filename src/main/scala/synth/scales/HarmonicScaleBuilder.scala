package synth.scales

import synth.{HarmonicSeries, NoteSeries}
import synth.NoteSeries.Interval

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 10/17/13
 * Time: 11:57 PM
 * To change this template use File | Settings | File Templates.
 */
object HarmonicScaleBuilder extends SeriesBuilder with BuildsHeptotonic {

  def fullIntervalPattern: Seq[IntervalPattern.Interval] = SeriesBuilder.toIntervals(Seq(1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 2))

  def fullPattern: IntervalPattern = IntervalPattern(for (i <- 1 to 12) yield 1)

  def getSeries: (Float) => NoteSeries = HarmonicSeries(_)

  def getFullIntervals: (NoteSeries) => Seq[Interval] =
    series => series match {
      case s: HarmonicSeries => HarmonicSeries.sorted13(s)
      case _ => throw sys.error("must be a Harmonic Series")
    }

  def heptoIntervalPattern: Seq[Int] = Seq(2, 2, 1, 2, 2, 2, 1)

  def getHeptoIntervals: (NoteSeries) => Seq[Interval] =
    series => {
      val notes = getFullIntervals(series)
      heptoIntervalPattern.scanLeft(0)(_+_) map { notes(_) }
    }
}
