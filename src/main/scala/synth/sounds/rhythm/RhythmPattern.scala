package synth.sounds.rhythm

import util.expr.Fraction

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 11/5/13
 * Time: 7:48 PM
 * To change this template use File | Settings | File Templates.
 */
class RhythmPattern protected(val meter: Fraction, val beats: Seq[Fraction]) {

  /*
   * Returns a new pattern which is offset * meter.denominator skewed/wrapped from this.
   */
  def mode(offset: Int): RhythmPattern = {
    val bs = beats map {
      b => b.plus(Fraction(offset, meter.denom)) match {
        case f: Fraction => f
        case _ => throw sys.error("Fraction plus Fraction returned a non-fraction")
      }
    } map {
      b => RhythmPattern.wrapBeat(meter, b)
    }
    RhythmPattern(meter, bs)
  }
}


object RhythmPattern {

  def apply(meter: Fraction, beats: Seq[Fraction]): RhythmPattern =
    if (beats.forall(b => b.toFloat < meter.toFloat && b.toFloat >= 0))
      new RhythmPattern(meter, beats)
    else
      throw sys.error("All beats must be between 0 and meter length")

  protected def wrapBeat(meter: Fraction, beat: Fraction): Fraction =
    if (beat.toFloat < meter)
      beat
    else
      beat.minus(meter) match {
        case f: Fraction => wrapBeat(meter, f)
        case _ => throw sys.error("Fraction minus Fraction returned a non-fraction")
      }
}
