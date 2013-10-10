package synth.scales

import synth.NoteSeries.Interval
import synth.scale.Note

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 10/10/13
 * Time: 1:41 PM
 * To change this template use File | Settings | File Templates.
 */
trait ScaleWithSteps[S <: ScaleInterface] extends ScaleInterface {

  def stepPattern: Seq[Int]

  def buildScale: Seq[Int] => (Seq[Interval], Seq[Note]) => S

  def applyPattern(steps: Seq[Int]): S = {
    val is = intervals.head +: applyPattern(intervals, stepPattern, steps).get
    val ns = notes.head +: applyPattern(notes, stepPattern, steps).get

    buildScale(steps)(is, ns)
  }

  private def applyPattern[T](is: Seq[T], oldPattern: Seq[Int], newPattern: Seq[Int]): Option[Seq[T]] = {
    newPattern match {
      case Seq() => Some(Seq())
      case s => {
        val skip = firstToSumTo(newPattern.head, oldPattern)

        skip match {
          case None => None
          case Some(skipThis) => {
            val nis = is.drop(skipThis.size)

            applyPattern( nis, oldPattern.drop(skipThis.size), newPattern.tail ) match {
              case None => None
              case Some(ss) => Some(is.head +: ss)
            }
          }
        }
      }
    }
  }

  private def firstToSumTo(x: Int, ss: Seq[Int]): Option[Seq[Int]] = {
    if (x > 0)
      firstToSumTo(x - ss.head, ss.tail).map {
        ss.head +: _
      }
    else if (x == 0)
      Some(Seq())
    else
      None
  }
}
