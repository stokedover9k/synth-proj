package synth.scales

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 10/10/13
 * Time: 2:37 PM
 * To change this template use File | Settings | File Templates.
 */

abstract class BasicNote {
  def name: Char

  def toFlat: BasicNote
  def toSharp: BasicNote

  def upHalf: BasicNote = upHalfS
  def downHalf: BasicNote = downHalfS
  def upWhole: BasicNote = upWholeS
  def downWhole: BasicNote = downWholeS

  def upHalfS: BasicNote
  def upHalfF: BasicNote

  def downHalfS: BasicNote
  def downHalfF: BasicNote

  def upWholeS: BasicNote
  def upWholeF: BasicNote

  def downWholeS: BasicNote
  def downWholeF: BasicNote

  override def toString = name.toString
}

object BasicNote {
  def next(name: Char) = ((name - 'A' + 1) % 7 + 'A').toChar
  def prev(name: Char) = ((name - 'A' + 6) % 7 + 'A').toChar
}

class Whole (override val name: Char) extends BasicNote {
  def toFlat: Flat = throw sys.error("Whole Notes can't be cast to flats")
  def toSharp: Sharp = throw sys.error("Whole Notes can't be cast to sharps")

  def upHalfS = new Sharp(name)
  def upHalfF = upHalfS.toFlat

  def downHalfF = new Flat(name)
  def downHalfS = downHalfF.toSharp

  def upWholeS: BasicNote = new Whole( BasicNote.next(name) )
  def upWholeF: BasicNote = upWholeS

  def downWholeS: BasicNote = new Whole( BasicNote.prev(name) )
  def downWholeF: BasicNote = downHalfS
}

class Sharp (override val name: Char) extends BasicNote {
  override def toSharp = this
  override def toFlat = new Flat(BasicNote.next(name))

  override def upHalfS = new Whole( BasicNote.next(name) )
  override def upHalfF = upHalfS

  override def downHalfF = new Whole(name)
  override def downHalfS = downHalfF

  override def upWholeS: BasicNote = new Sharp( BasicNote.next(name) )
  override def upWholeF: BasicNote = upWholeS.toFlat

  override def downWholeS: BasicNote = downWholeF.toSharp
  override def downWholeF: BasicNote = new Flat( name )

  override def toString = super.toString + "#"
}

class Flat (override val name: Char) extends BasicNote {
  override def toSharp = new Sharp(BasicNote.prev(name))
  override def toFlat = this

  override def upHalfS: BasicNote = new Whole(name)
  override def upHalfF: BasicNote = upHalfS

  override def downHalfS: BasicNote = new Whole( BasicNote.prev(name) )
  override def downHalfF: BasicNote = downHalfS

  override def upWholeS: BasicNote = new Sharp( name )
  override def upWholeF: BasicNote = upWholeS.toFlat

  override def downWholeS: BasicNote = downWholeF.toSharp
  override def downWholeF: BasicNote = new Flat( BasicNote.prev(name) )

  override def toString = super.toString + "b"
}
