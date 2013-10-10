package synth.scales

import synth.NoteSeries.Interval
import synth.scale.Note

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 10/8/13
 * Time: 10:32 PM
 * To change this template use File | Settings | File Templates.
 */

abstract class ScaleInterface {

  def intervals: Seq[Interval]

  def interval(index: Int)    : Interval

  def interval(note: Note)  : Interval

  def notes: Seq[Note]

  def note(index: Int): Note

}


case class MyScale(
                    intervals: Seq[Interval],
                    notes: Seq[Note]
                    ) extends ScaleInterface {

  def interval(index: Int): Interval = intervals(index)

  def interval(note: Note): Interval = interval(notes indexOf note)

  def note(index: Int): Note = notes(index)
}
