package synth

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 10/7/13
 * Time: 4:53 PM
 * To change this template use File | Settings | File Templates.
 */

/*

object Notes {
  case class Name protected(override val toString: String) {
    def octave = Name("[" + toString + "]")
  }

  object A extends Name("A")
  object B extends Name("B")
  object C extends Name("C")
  object D extends Name("D")
  object E extends Name("E")
  object F extends Name("F")
  object G extends Name("G")

  object Ab extends Name("Ab")
  object Bb extends Name("Bb")
  object Db extends Name("Db")
  object Eb extends Name("Eb")
  object Gb extends Name("Gb")

  object As extends Name("A#")
  object Cs extends Name("C#")
  object Ds extends Name("D#")
  object Fs extends Name("F#")
  object Gs extends Name("G#")

  lazy val tones:           IndexedSeq[Name] = IndexedSeq(A,     B, C,     D,     E, F,     G)
  lazy val tonesWithSharps: IndexedSeq[Name] = IndexedSeq(A, As, B, C, Cs, D, Ds, E, F, Fs, G, Gs)
  lazy val tonesWithFlats:  IndexedSeq[Name] = IndexedSeq(A, Bb, B, C, Db, D, Eb, E, F, Gb, G, Ab)

  def startingWith(name: Name, names: IndexedSeq[Name]): IndexedSeq[Name] = {
    val (head, tail) = names.splitAt( names.indexOf(name) )
    tail ++ head
  }
}

trait NamedNotes {
  def getInterval(noteName: Notes.Name): NoteSeries.Interval
}

trait NamedWholeTones extends NamedNotes {

  def A: NoteSeries.Interval = getInterval(Notes.A)
  def B: NoteSeries.Interval = getInterval(Notes.B)
  def C: NoteSeries.Interval = getInterval(Notes.C)
  def D: NoteSeries.Interval = getInterval(Notes.D)
  def E: NoteSeries.Interval = getInterval(Notes.E)
  def F: NoteSeries.Interval = getInterval(Notes.F)
  def G: NoteSeries.Interval = getInterval(Notes.G)

}

trait NamedSharps extends NamedNotes {

  def As: NoteSeries.Interval = getInterval(Notes.As)
  def Cs: NoteSeries.Interval = getInterval(Notes.Cs)
  def Ds: NoteSeries.Interval = getInterval(Notes.Ds)
  def Fs: NoteSeries.Interval = getInterval(Notes.Fs)
  def Gs: NoteSeries.Interval = getInterval(Notes.Gs)
}

trait NamedFlats extends NamedNotes {

  def Ab: NoteSeries.Interval = getInterval(Notes.Ab)
  def Bb: NoteSeries.Interval = getInterval(Notes.Bb)
  def Db: NoteSeries.Interval = getInterval(Notes.Db)
  def Eb: NoteSeries.Interval = getInterval(Notes.Eb)
  def Gb: NoteSeries.Interval = getInterval(Notes.Gb)
 
}

trait FlatsAreSharps extends NamedFlats with NamedSharps {

  override def Ab = Gs
  override def Bb = As
  override def Db = Cs
  override def Eb = Ds
  override def Gb = Fs

}

trait SharpsAreFlats extends NamedSharps with NamedFlats {

  override def Gs = Ab
  override def As = Bb
  override def Cs = Db
  override def Ds = Eb
  override def Fs = Gb

}

*/