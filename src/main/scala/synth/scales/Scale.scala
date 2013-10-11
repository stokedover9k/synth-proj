package synth.scales


class Scale(notes: Seq[Note]) extends Modes[Scale] {

  protected def builder: ModeBuilder[Scale] = new ModeCutter[Scale] {
    def cutSequenceForMode[T](seq: Seq[T], offset: Int): (Seq[T], Seq[T]) = seq.splitAt(offset)

    def notes: Seq[Note] = Scale.this.notes

    def build(notes: Seq[Note]): Scale = new Scale(notes)
  }

  override def toString: String = notes.mkString("Scale:[", " ", "]")
}


class OctaveScale(notes: Seq[Note]) extends Scale(notes) with Modes[OctaveScale] {

  override protected def builder: ModeBuilder[OctaveScale] = new ModeCutter[OctaveScale] {
    def cutSequenceForMode[T](seq: Seq[T], offset: Int): (Seq[T], Seq[T]) =
      (seq.tail.take(offset), seq.drop(offset))

    def notes: Seq[Note] = OctaveScale.this.notes

    def build(notes: Seq[Note]): OctaveScale = new OctaveScale(notes)
  }
}


class Scale7 protected(notes: Seq[Note]) extends OctaveScale(notes) {

  def Ionian = mode(0)
  def Dorian = mode(1)
  def Phrygian = mode(2)
  def Lydian = mode(3)
  def Mixolydian = mode(4)
  def Aeolian = mode(5)
  def Locrian = mode(6)
  def IonianOctave = mode(7)

}

object Scale7 {
  def apply(notes: Seq[Note]): Scale7 = {
    notes.size match {
      case 7 => apply( notes :+ notes.head.octaveUp )
      case 8 => new Scale7(notes)
      case s => sys.error("Attempting to construct a Scale7 (heptatonic) with %d notes (needs 7 or 8).".format(s))
    }
  }
}
