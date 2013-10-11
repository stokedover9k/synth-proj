package synth.scales

import synth.NoteSeries

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 10/10/13
 * Time: 5:08 PM
 * To change this template use File | Settings | File Templates.
 */

case class Note(note: BasicNote, interval: NoteSeries.Interval) {
  override def toString: String = "%s%d[%.2f]".format(note.toString, interval.octave, interval.hz)

  def octaveUp: Note = Note(note, interval.octaveUp)
}
