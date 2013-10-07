package synth

import synth.NoteSeries.Interval
import scala.collection.mutable

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 10/7/13
 * Time: 3:04 PM
 * To change this template use File | Settings | File Templates.
 */

trait ExtractsUniqueIntervals {

  /*
   * Returns a sequence of unique intervals.
   */
  def extract(n: Int): Iterable[NoteSeries.Interval]
}

case class IntervalSequenceBuilder(series: NoteSeries) extends ExtractsUniqueIntervals {

  /*
   * Returns a sequence of unique intervals.  These are the first n unique intervals in the series.
   */
  def extract(n: Int): Iterable[Interval] = {
    val intervals = new mutable.HashMap[Float, NoteSeries.Interval]
    var i = 0
    while( intervals.size < n ) {
      val interval = series(i)
      if( !intervals.contains(interval.hz) )
        intervals.put(interval.hz, interval)
      i += 1
    }
    intervals.values
  }
}
