package synth

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 10/8/13
 * Time: 1:00 AM
 * To change this template use File | Settings | File Templates.
 */

trait Modes[S <: MyScale] {

  def modeAt(offset: Int): S
}



trait MakesModes[S <: ScaleWithNames] extends ScaleWithNames with MakesScalesFromNamedIntervals[S] {

  def modeAt(offset: Int): S = {
    val (head, tail) = intervals.splitAt(offset)
    val newIntervals = tail ++ (head map { case NamedInterval(name, interval) => NamedInterval(name, interval.octaveUp)} )

    buildFromNamedIntervals(newIntervals)
  }

}



trait WesternModes[S <: MyScale] extends MakesModes[S] {

  protected def nthModeOffset(n: Int): Int

  def Ionian       : S = modeAt( nthModeOffset(0) )
  def Dorian       : S = modeAt( nthModeOffset(1) )
  def Phrygian     : S = modeAt( nthModeOffset(2) )
  def Lydian       : S = modeAt( nthModeOffset(3) )
  def Mixolydian   : S = modeAt( nthModeOffset(4) )
  def Aeolian      : S = modeAt( nthModeOffset(5) )
  def Locrian      : S = modeAt( nthModeOffset(6) )
  def IonianOctave : S = modeAt( nthModeOffset(7) )

}
