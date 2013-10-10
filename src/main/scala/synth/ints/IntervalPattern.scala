package synth.ints

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 10/10/13
 * Time: 3:07 PM
 * To change this template use File | Settings | File Templates.
 */

object IntervalPattern {

  abstract class Interval {
    def apply: Note => Note
  }

  object HalfStep extends Interval {
    def apply: (Note) => Note = _.upHalf
  }

  object WholeStep extends Interval {
    def apply: (Note) => Note = _.upWhole
  }

  object CommaStep extends Interval {
    def apply: (Note) => Note = _.toFlat
  }

  def getNotes(note: Note, pattern: Seq[Interval]): Seq[Note] = {
    pattern match {
      case Seq() => Seq(note)
      case s => note +: getNotes(pattern.head.apply(note), pattern.tail)
    }
  }

  def main(args: Array[String]): Unit = {
    val startNote = new Whole('C')
    val pattern = Seq(HalfStep, HalfStep, WholeStep, WholeStep, HalfStep, CommaStep, HalfStep)

    println(getNotes(startNote, pattern))
  }

}
