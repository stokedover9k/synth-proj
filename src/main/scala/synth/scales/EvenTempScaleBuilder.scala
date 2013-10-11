package synth.scales

import IntervalPattern.Interval
import synth.{EvenTempSeries, NoteSeries}

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 10/11/13
 * Time: 1:04 PM
 * To change this template use File | Settings | File Templates.
 */
object EvenTempScaleBuilder extends SeriesBuilder {
  def fullIntervalPattern: Seq[Interval] = SeriesBuilder.toIntervals(Seq(1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 2))

  def fullPattern: IntervalPattern = IntervalPattern(Seq(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))

  def getSeries: (Float) => NoteSeries = EvenTempSeries(_)

  def getFullIntervals: (NoteSeries) => Seq[NoteSeries.Interval] =
    series => (0 to 11 map (series(_))) :+ series(12).octaveUp
}
