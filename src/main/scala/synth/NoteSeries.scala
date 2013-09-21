package synth

import util.{Log2, Fraction}

/**
 * Created with IntelliJ IDEA.
 * User: yuriy
 * Date: 9/21/13
 * Time: 12:09 AM
 * To change this template use File | Settings | File Templates.
 */

abstract class NoteSeries {

  /*
   * The series' fundamental frequency.
   */
  def fundamental: Float

  /*
   * Returns the interval of the specified degree within the series.
   */
  def apply(degree: Int): NoteSeries.Interval
}



object NoteSeries {

  abstract class Interval {

    /*
     * Interval's degree within the series.
     */
    def degree: Int

    /*
     * Fundamental frequency of the interval's series.
     */
    def fundamental: Float

    /*
     * Frequency of the Interval within an octave of the fundamental.
     */
    def hz: Float = fundamental * hzDecimalRatio

    /*
     * Basic (not octave-adjusted) ratio to the fundamental's frequency.
     */
    def hzSimpleRatio: Fraction

    /*
     * Basic (before octave-adjustment) frequency of the interval
     */
    def hzSimple: Float = hzSimpleRatio.toFloat * fundamental

    /*
     * Octave (in respect to the fundamental) into which the interval falls before adjustment
     */
    def octave: Int = Log2(Math.floor(hzSimpleRatio.toFloat)) + 1

    /*
     * Dividing the basic frequency (hzSimple) by this factor puts it within the fundamental's octave.
     */
    def adjustFactor: Int = 1 << Log2(Math.floor(hzSimpleRatio.toFloat))

    /*
     * Ratio to the fundamental with adjustment into its octave.
     */
    def hzAdjustedRatio: Fraction = hzSimpleRatio / adjustFactor

    /*
     * Ratio to the fundamental with adjustment into its octave reduced to a proper factor form.
     */
    def hzReducedRatio: Fraction = hzAdjustedRatio.reduce

    /*
     * Ratio to the fundamental with adjustment into its octave in a decimal format.
     */
    def hzDecimalRatio: Float = hzAdjustedRatio.toFloat
  }
}
