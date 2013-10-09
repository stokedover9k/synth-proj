package synth.scales

import synth.NoteSeries.Interval

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

  def interval(note: String)  : Interval

  def notes: Seq[String]

  def note(index: Int): String

}


case class MyScale(
                    intervals: Seq[Interval],
                    notes: Seq[String]
                    ) extends ScaleInterface {

  def interval(index: Int): Interval = intervals(index)

  def interval(note: String): Interval = interval(notes indexOf note)

  def note(index: Int): String = notes(index)
}
