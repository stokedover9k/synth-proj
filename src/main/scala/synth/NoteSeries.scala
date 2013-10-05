package synth

import util.expr.{Fraction, WholeNum, Expr}

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
     * The degree of the interval in the series.
     */
    def degree: Int

    /*
     * The fundamental frequency of the series.
     */
    def fundamental: Float

    /*
     * The expression multiplied by the fundamental to get the interval's frequency.
     * This frequency is not expected to be within the fundamental frequency's octave.
     */
    def generatingExpression: Expr

    /*
     * Frequency of the interval created by the generating expression.
     */
    def hzUnscaled: Float = generatingExpression.toFloat * fundamental

    /*
     * The octave of the unscaled frequency (0 indexed).
     */
    def octave: Int = (Math.log(hzUnscaled / fundamental) / Math.log(2)).floor.toInt

    /*
     * The factor by which the unscaled frequency is divided to adjust it into
     * the fundamental's octave.
     */
    def octaveAdjustment: Expr = Fraction(2, 1).pow(WholeNum(octave))

    /*
     * The ratio of the fundamental to the frequency.
     */
    def hzFactor: Expr = generatingExpression.div(octaveAdjustment)

    /*
     * Frequency of the interval within the fundamental's frequency.
     */
    def hz: Float = hzFactor.toFloat * fundamental

    /*
     * Interval of the same degree in the next octave.
     */
    def inNextOctave: Interval
  }
}
