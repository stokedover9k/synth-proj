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
    val dif = beats(offset)
    val bs = beats splitAt offset match {
      case (head, tail) => {
        def h = tail map (_.minus(dif))
        def t = head map (_.plus(meter).minus(dif))
        (h ++ t) map {
          case f: Fraction => f
          case _ => throw sys.error("Fraction operations returned a non-fraction")
        }
      }
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
