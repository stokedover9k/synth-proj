package synth

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 9/14/13
 * Time: 12:55 AM
 * To change this template use File | Settings | File Templates.
 */

abstract class SoundWave(pos: Double) {

  /*
   * Amplitude
   */
  val amp: Double

  /*
   * Measurement at this.pos + delta
   */
  def next(delta: Double): SoundWave
}



case class SineWave(pos: Double) extends SoundWave(pos) {

  lazy val amp: Double = Math.sin(pos)

  def next(delta: Double): SineWave = {
    val nextPos = pos + delta
    val nextPosCapped = if( nextPos < Math.PI * 2 ) nextPos else nextPos - Math.PI * 2
    new SineWave( nextPosCapped )
  }
}
