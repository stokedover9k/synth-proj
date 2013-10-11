package synth.ints

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 10/10/13
 * Time: 4:28 PM
 * To change this template use File | Settings | File Templates.
 */

object PythagBuild {

  private val fullPattern = Seq(1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 1)

  private val fullIntervalPattern = toIntervals(fullPattern)

  private val heptoPattern = Seq(2, 2, 1, 2, 2, 2, 1)

  private val heptoIntervalPattern = toIntervals(heptoPattern)

  private def toIntervals(pattern: Seq[Int]): Seq[IntervalPattern.Interval] = {
    pattern map {
      _ match {
        case 2 => IntervalPattern.WholeStep
        case 1 => IntervalPattern.HalfStep
        case 0 => IntervalPattern.CommaStep
        case _ => throw sys.error("oops")
      }
    }
  }

  private lazy val fullPatternNotes = IntervalPattern.getNotes(new Whole('C'), fullIntervalPattern)

  def tagWithOctave(notes: Seq[BasicNote]): Seq[Int] = {
    val first = notes.head
    var octave = 0

    def toOctave(n: BasicNote) = {
      if (n.toString == first.toString) octave = octave + 1
      octave
    }

    notes map toOctave
  }



  def main(args: Array[String]): Unit = {

    println(fullPatternNotes)

    (fullPatternNotes, tagWithOctave(fullPatternNotes)).zipped.foreach( (a,b) => print(a.toString + b + " ") )
    println()
  }
}
