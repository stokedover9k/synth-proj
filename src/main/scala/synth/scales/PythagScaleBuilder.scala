package synth.scales

import synth.{EvenTempSeries, NoteSeries, PythagoreanSeries}
import synth.scales.IntervalPattern.Interval

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 10/10/13
 * Time: 4:28 PM
 * To change this template use File | Settings | File Templates.
 */

object PythagScaleBuilder extends SeriesBuilder with BuildsHeptotonic {

  def heptoIntervalPattern: Seq[Int] = Seq(2, 2, 1, 2, 2, 2, 1)

  def fullIntervalPattern: Seq[Interval] = SeriesBuilder.toIntervals(Seq(1, 1, 1, 1, 2, 1, 0, 1, 1, 1, 1, 1, 2))

  def fullPattern: IntervalPattern = IntervalPattern(Seq(1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1))

  def getSeries: (Float) => NoteSeries = PythagoreanSeries(_)

  def getFullIntervals: (NoteSeries) => Seq[NoteSeries.Interval] =
    series => (-1 to 11 map (series(_))).sortBy(_.hz) :+ series(12)

  def getHeptoIntervals: (NoteSeries) => Seq[NoteSeries.Interval] =
    series => (-1 to 5 map (series(_))).sortBy(_.hz) :+ series(0)

}
