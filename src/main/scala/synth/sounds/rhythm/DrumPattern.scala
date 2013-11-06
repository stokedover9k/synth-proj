package synth.sounds.rhythm

import util.expr.Fraction
import synth.sounds.ClipCollection

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 11/5/13
 * Time: 7:49 PM
 * To change this template use File | Settings | File Templates.
 */
class DrumPattern(rhythm: RhythmPattern, drumClip: Array[Byte])
  extends RhythmPattern(rhythm.meter, rhythm.beats) {

  override def mode(offset: Int): DrumPattern =
    new DrumPattern(rhythm.mode(beats(offset % beats.size).num), drumClip)

  /*
   * @tempo is the number of beats (1/meter.denominator) per minute
   */
  def getClipsAt(tempo: Int, sampleRate: Float): ClipCollection = {

    def secOffset(beat: Fraction): Float = 60f * (beat.div(Fraction.improper(tempo, meter.denom)))

    val offsets = beats map (b => (secOffset(b) * sampleRate).toInt)

    offsets.foldLeft(ClipCollection()) {
      (cc, offset) => cc.addClip(drumClip, offset)
    }
  }
}

object DrumPatterns {

  def afrikanPattern: RhythmPattern = {
    val beats = Seq(0, 2, 4, 5, 7, 9, 11) map (b => Fraction.improper(b, 8))
    RhythmPattern(Fraction.improper(12, 8), beats)
  }
}
