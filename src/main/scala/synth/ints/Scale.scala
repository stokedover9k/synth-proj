package synth.ints

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


abstract class AbsScale {
  def notes: Seq[Note]
}


class Scale(override val notes: Seq[Note]) extends AbsScale with Modes[Scale] {

  protected def builder: ModeBuilder[Scale] = new ModeCutter[Scale] {
    def cutSequenceForMode[T](seq: Seq[T], offset: Int): (Seq[T], Seq[T]) = seq.splitAt(offset)

    def notes: Seq[Note] = Scale.this.notes

    def build(notes: Seq[Note]): Scale = new Scale(notes)
  }
}


class OctaveScale(notes: Seq[Note]) extends Scale(notes) with Modes[OctaveScale] {

  override protected def builder: ModeBuilder[OctaveScale] = new ModeCutter[OctaveScale] {
    def cutSequenceForMode[T](seq: Seq[T], offset: Int): (Seq[T], Seq[T]) =
      (seq.tail.take(offset), seq.drop(offset))

    def notes: Seq[Note] = OctaveScale.this.notes

    def build(notes: Seq[Note]): OctaveScale = new OctaveScale(notes)
  }
}


trait ModeBuilder[+S <: Scale] {
  def build(offset: Int): S
}


trait ModeCutter[+S <: Scale] extends ModeBuilder[S] {

  def cutSequenceForMode[T](seq: Seq[T], offset: Int): (Seq[T], Seq[T])

  def notes: Seq[Note]

  def build(notes: Seq[Note]): S

  def build(offset: Int): S = {
    cutSequenceForMode(notes, offset) match {
      case (h, t) => build(t ++ (h map (_.octaveUp)))
    }
  }
}


trait Modes[+ScaleType <: Scale] {

  def mode(offset: Int): ScaleType = builder.build(offset)

  protected def builder: ModeBuilder[ScaleType]
}
