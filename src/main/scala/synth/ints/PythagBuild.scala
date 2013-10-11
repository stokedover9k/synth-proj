package synth.ints

import synth.{PythagoreanSeries, NoteSeries}

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 10/10/13
 * Time: 4:28 PM
 * To change this template use File | Settings | File Templates.
 */

object PythagBuild {

  private lazy val fullIntervalPattern = toIntervals(Seq(1, 1, 1, 1, 2, 1, 0, 1, 1, 1, 1, 1, 2))

  private lazy val fullPatternNotes = IntervalPattern.getNotes(new Whole('C'), fullIntervalPattern)

  private lazy val fullPattern = IntervalPattern(Seq(1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1))

  private lazy val heptoIntervalPattern = Seq(2, 2, 1, 2, 2, 2, 1)

  private lazy val heptoPatternNotes = fullPattern.getNotes(fullPatternNotes, heptoIntervalPattern)

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

  def tagWithOctave(notes: Seq[BasicNote]): Seq[Int] = {
    val first = notes.head
    var octave = -1  // start at -1, because the very first note will raise it to 0

    def toOctave(n: BasicNote) = {
      if (n.toString == first.toString) octave = octave + 1
      octave
    }

    notes map toOctave
  }

  def intervalToOctave(interval: NoteSeries.Interval, octave: Int): NoteSeries.Interval = {
    if (octave < interval.octave)
      throw sys.error("Trying to lower an Interval's octave.")
    else if (interval.octave == octave)
      interval
    else
      intervalToOctave(interval.octaveUp, octave)
  }

  def tagOctaves(notes: Seq[BasicNote]) =
    (notes, tagWithOctave(notes)).zipped

  private def toNotes(intervals: Seq[NoteSeries.Interval], notes: Seq[BasicNote]): Seq[Note] = {
    val tagged = tagOctaves(notes).toSeq
    (intervals, tagged).zipped.map {
      case (i: NoteSeries.Interval, note) => {
        note match {
          case (n: BasicNote, o: Int) => Note(n, intervalToOctave(i, o))
        }
      }
    }
  }

  def fullIntervals(fundamental: Float): Seq[Note] = {
    val series = PythagoreanSeries(fundamental)
    val intervals: Seq[NoteSeries.Interval] = {
      (-1 to 11 map (series(_))).sortBy(_.hz) :+ series(12)  // add octave after sorting so that it stays at the end
    }
    toNotes(intervals, fullPatternNotes)
  }

  def heptoIntervals(fundamental: Float): Seq[Note] = {
    val series = PythagoreanSeries(fundamental)
    val intervals: Seq[NoteSeries.Interval] =
      (-1 to 5 map (series(_))).sortBy(_.hz) :+ series(12)  // add octave after sorting so thatit stays at the end
    toNotes(intervals, heptoPatternNotes)
  }

  def main(args: Array[String]): Unit = {

    println(fullPatternNotes)
    tagOctaves(fullPatternNotes).foreach {
      (a, b) => print(a.toString + b + " ")
    }
    println()

    tagOctaves(heptoPatternNotes).foreach {
      (a, b) => print(a.toString + b + " ")
    }
    println()

    println(fullIntervals(528f))
    println(heptoIntervals(528f))
  }
}
