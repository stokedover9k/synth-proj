package synth.ints

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 10/10/13
 * Time: 2:37 PM
 * To change this template use File | Settings | File Templates.
 */

abstract class Note {
  def name: Char

  def toFlat: Note
  def toSharp: Note

  def upHalf = upHalfS
  def downHalf = downHalfS
  def upWhole = upWholeS
  def downWhole = downWholeS

  def upHalfS: Note
  def upHalfF: Note

  def downHalfS: Note
  def downHalfF: Note

  def upWholeS: Note
  def upWholeF: Note

  def downWholeS: Note
  def downWholeF: Note

  override def toString = name.toString
}

object Note {
  def next(name: Char) = ((name - 'A' + 1) % 7 + 'A').toChar
  def prev(name: Char) = ((name - 'A' + 6) % 7 + 'A').toChar
}

class Whole (override val name: Char) extends Note {
  def toFlat: Flat = throw sys.error("Whole Notes can't be cast to flats")
  def toSharp: Sharp = throw sys.error("Whole Notes can't be cast to sharps")

  def upHalfS = new Sharp(name)
  def upHalfF = upHalfS.toFlat

  def downHalfF = new Flat(name)
  def downHalfS = downHalfF.toSharp

  def upWholeS: Note = new Whole( Note.next(name) )
  def upWholeF: Note = upWholeS

  def downWholeS: Note = new Whole( Note.prev(name) )
  def downWholeF: Note = downHalfS
}

class Sharp (override val name: Char) extends Note {
  override def toSharp = this
  override def toFlat = new Flat(Note.next(name))

  override def upHalfS = new Whole( Note.next(name) )
  override def upHalfF = upHalfS

  override def downHalfF = new Whole(name)
  override def downHalfS = downHalfF

  override def upWholeS: Note = new Sharp( Note.next(name) )
  override def upWholeF: Note = upWholeS.toFlat

  override def downWholeS: Note = downWholeF.toSharp
  override def downWholeF: Note = new Flat( name )

  override def toString = super.toString + "#"
}

class Flat (override val name: Char) extends Note {
  override def toSharp = new Sharp(Note.prev(name))
  override def toFlat = this

  override def upHalfS: Note = new Whole(name)
  override def upHalfF: Note = upHalfS

  override def downHalfS: Note = new Whole( Note.prev(name) )
  override def downHalfF: Note = downHalfS

  override def upWholeS: Note = new Sharp( name )
  override def upWholeF: Note = upWholeS.toFlat

  override def downWholeS: Note = downWholeF.toSharp
  override def downWholeF: Note = new Flat( Note.prev(name) )

  override def toString = super.toString + "b"
}
