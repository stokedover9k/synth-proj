package synth.scales

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 10/11/13
 * Time: 2:15 AM
 * To change this template use File | Settings | File Templates.
 */

trait Modes[+ScaleType <: Scale] {

  def mode(offset: Int): ScaleType = builder.build(offset)

  protected def builder: ModeBuilder[ScaleType]
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
