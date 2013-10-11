package synth.scales

import synth.NoteSeries

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 10/11/13
 * Time: 12:54 PM
 * To change this template use File | Settings | File Templates.
 */

abstract class SeriesBuilder {

  /*
   * A sequence of Interval's used to pick notes from a sequence of note names
   * containing all whole tones, sharps, and flats (yes, even things like E#, Fb).
   * Note that because of this, the step from E to F counts as a whole step.
   * IntervalPattern class provides a convenient function for this (toIntervals).
   */
  def fullIntervalPattern: Seq[IntervalPattern.Interval]

  /*
   * Set of notes in the full scale.
   */
  protected lazy val fullNoteSet = IntervalPattern.getNotes(new Whole('C'), fullIntervalPattern)

  /*
   * The step pattern of the full scale. Here, the step between E and F may be considered
   * a half step.
   */
  def fullPattern: IntervalPattern

  /*
   * Given a fundamental, returns a proper series.
   */
  def getSeries: Float => NoteSeries

  /*
   * Tells the builder how to extract a sequence of notes from a particular series.
   * This function will be given the series returned by getSeries: Float => NoteSeries
   * and is expected to return the Intervals in order from which a full (octave-wrapped)
   * scale is constructed.
   */
  def getFullIntervals: NoteSeries => Seq[NoteSeries.Interval]

  /*
   * Gets notes for a full scale from a series constructed with the given fundamental.
   */
  private def fullScaleNotes(fundamental: Float): Seq[Note] = {
    val intervals: Seq[NoteSeries.Interval] =
      getFullIntervals(getSeries(fundamental))
    SeriesBuilder.toNotes(intervals, fullNoteSet)
  }

  /*
   * Creates a full scale with a given fundamental.
   */
  def fullScale(fundamental: Float): OctaveScale = new OctaveScale(fullScaleNotes(fundamental))

}


trait BuildsHeptotonic extends SeriesBuilder {
  def heptoIntervalPattern: Seq[Int]

  private lazy val heptoNoteSet = fullPattern.getNotes(fullNoteSet, heptoIntervalPattern)

  /*
   * Tells the builder how to extract a sequence of notes from a particular series.
   * This function will be given the series returned by getSeries: Float => NoteSeries
   * and is expected to return the Intervals in order from which a heptatonic
   * (octave-wrapped) scale is constructed.
   */
  def getHeptoIntervals: NoteSeries => Seq[NoteSeries.Interval]

  /*
   * Gets notes for a heptatonic scale from a series constructed with the given fundamental.
   */
  private def heptoIntervals(fundamental: Float): Seq[Note] = {
    val intervals: Seq[NoteSeries.Interval] =
      getHeptoIntervals(getSeries(fundamental))
    SeriesBuilder.toNotes(intervals, heptoNoteSet)
  }

  /*
   * Creates a heptatonic scale with a given fundamental.
   */
  def heptoScale(fundamental: Float): Scale7 = Scale7(heptoIntervals(fundamental))

}


object SeriesBuilder {

  def toIntervals(pattern: Seq[Int]): Seq[IntervalPattern.Interval] = {
    pattern map {
      _ match {
        case 2 => IntervalPattern.WholeStep
        case 1 => IntervalPattern.HalfStep
        case 0 => IntervalPattern.CommaStep
        case _ => throw sys.error("oops")
      }
    }
  }

  private def tagWithOctave(notes: Seq[BasicNote]): Seq[Int] = {
    val first = notes.head
    var octave = -1 // start at -1, because the very first note will raise it to 0

    def toOctave(n: BasicNote) = {
      if (n.toString == first.toString) octave = octave + 1
      octave
    }

    notes map toOctave
  }

  private def intervalToOctave(interval: NoteSeries.Interval, octave: Int): NoteSeries.Interval = {
    if (octave < interval.octave)
      throw sys.error("Trying to lower an Interval's octave.")
    else if (interval.octave == octave)
      interval
    else
      intervalToOctave(interval.octaveUp, octave)
  }

  private def tagOctaves(notes: Seq[BasicNote]) =
    (notes, SeriesBuilder.tagWithOctave(notes)).zipped

  def toNotes(intervals: Seq[NoteSeries.Interval], notes: Seq[BasicNote]): Seq[Note] = {
    val tagged = tagOctaves(notes).toSeq
    (intervals, tagged).zipped.map {
      case (i: NoteSeries.Interval, note) => {
        note match {
          case (n: BasicNote, o: Int) => Note(n, intervalToOctave(i, o))
        }
      }
    }
  }
}
