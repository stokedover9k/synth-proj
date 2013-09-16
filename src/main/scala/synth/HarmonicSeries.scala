package synth

/**
 * Created with IntelliJ IDEA.
 * User: yuriy
 * Date: 9/16/13
 * Time: 12:11 PM
 * To change this template use File | Settings | File Templates.
 */

case class HarmonicSeries(fundamental: Float) {

  case class Note(degree: Int, fundamental: Float) {
    def simpleFrequency: Float = fundamental * degree
    lazy val frequency = fundamental * ratioToFundamentalAdjustedReducedDecimal

    def ratioToFundamental = (degree, 1)
    def ratioToFundamentalAdjusted = (degree, octaveAdjustmentFactor)
    def ratioToFundamentalAdjustedReduced = {
      val g = util.gdc(degree, octaveAdjustmentFactor)
      (degree / g, octaveAdjustmentFactor / g)
    }
    def ratioToFundamentalAdjustedReducedDecimal =
      ratioToFundamentalAdjustedReduced match { case (x, y) => x.toFloat / y }
    def octave = util.Log2(degree) + 1
    def octaveAdjustmentFactor = 1 << util.Log2(degree)
  }

  def apply(degree: Int) = Note(degree, fundamental)
}
