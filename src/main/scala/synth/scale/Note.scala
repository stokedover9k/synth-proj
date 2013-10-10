package synth.scale

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 10/9/13
 * Time: 11:27 PM
 * To change this template use File | Settings | File Templates.
 */

case class Note protected(name: String, octave: Int) {

  def octaveUp = new Note(name, octave + 1)

  override def toString = name + (if (octave != 0) octave.toString else "")
}

object Note {
  private def checkName(name: String) =
    if (name.size == 1 && "ABCDEFG".indexOf(name) != -1)
      Some(name)
    else
      None

  def makeNote(name: String, octave: Int = 0): Option[Note] =
    checkName(name) map (new Note(_, octave))

  def makeSharp(name: String, octave: Int): Option[Note] =
    checkName(name) map (n => new Note(n + "#", octave))

  def makeFlat(name: String, octave: Int): Option[Note] =
    checkName(name) map (n => new Note(n + "b", octave))
}
