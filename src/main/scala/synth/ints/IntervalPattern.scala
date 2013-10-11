package synth.ints

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 10/10/13
 * Time: 3:07 PM
 * To change this template use File | Settings | File Templates.
 */

case class IntervalPattern(startPattern: Seq[Int]) {

  private def prefixSum(sum: Int, ns: Seq[Int]): Option[Seq[Int]] = {
    if (sum > ns.head || ns.head == 0)
      prefixSum(sum - ns.head, ns.tail) map (ns.head +: _)
    else if (sum == ns.head)
      Some(Seq(ns.head))
    else
      None
  }

  def getNotes[N](startNotes: Seq[N], pattern: Seq[Int]): Seq[N] = {

    def stuff(notes: Seq[N], ois: Seq[Int], nis: Seq[Int]): Option[Seq[N]] = {
      nis match {
        case Seq() => Some(Seq(notes.head))
        case _ =>
          prefixSum(nis.head, ois) map (_.size) match {
            case None => None
            case Some(s: Int) => {
              stuff(notes drop s, ois drop s, nis tail) map {
                notes.head +: _
              }
            }
          }
      }
    }

    stuff(startNotes, startPattern, pattern).getOrElse(
      sys.error("Cannot extract pattern %s from %s".format(pattern, startPattern))
    )
  }
}

object IntervalPattern {

  abstract class Interval {
    def apply: BasicNote => BasicNote
  }

  object HalfStep extends Interval {
    def apply: (BasicNote) => BasicNote = _.upHalf
  }

  object WholeStep extends Interval {
    def apply: (BasicNote) => BasicNote = _.upWhole
  }

  object CommaStep extends Interval {
    def apply: (BasicNote) => BasicNote = _.toFlat
  }

  def getNotes(note: BasicNote, pattern: Seq[Interval]): Seq[BasicNote] = {
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
