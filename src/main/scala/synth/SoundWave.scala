package synth

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 9/14/13
 * Time: 12:55 AM
 * To change this template use File | Settings | File Templates.
 */

trait Wave {
  def frequency: Float
  def at(t: Float): Float
}

trait CreatesWaves {
  def apply(frequency: Float): Wave
}

case class Sine(frequency: Float) extends Wave {
  def at(t: Float) = Math.sin(t * frequency * 2 * Math.PI).toFloat
}

object Sine extends CreatesWaves
