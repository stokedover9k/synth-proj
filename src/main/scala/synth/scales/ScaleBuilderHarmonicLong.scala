package synth.scales

import scala.collection.immutable
import synth.SeriesHarmonic
import synth.Series.Interval

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 10/22/13
 * Time: 2:10 AM
 * To change this template use File | Settings | File Templates.
 */

case class ScaleBuilderHarmonicLong(fundamental: Float, count: Int) {

  val series = SeriesHarmonic(fundamental)

  def allIntervals = 0 until count map series.apply

  def allTypes = for (i <- 1 to count) yield IntervalType.First

  def allNotes = for (i <- 1 to count) yield "C"

  def build(): TypedScale with ComparesToPythag = new TypedScale with ComparesToPythag {

    override val intervals: immutable.IndexedSeq[Interval] = allIntervals

    override val allNames: Seq[String] = {
      closestToPythagIndices.zipWithIndex.map(
        _ match {
          case (pythag, harm) => {
            val h = apply(harm).hz
            val p = pythagoreanScale(pythag).hz
            pythagoreanScale.getName(pythag) + (if (h < p) "-" else if (h > p) "+" else "")
          }
        }
      )
    }

    override val allTypes: Seq[IntervalType] = {
      closestToPythagIndices map pythagoreanScale.getType //.zipWithIndex.map(
    }
  }
}


trait ComparesToPythag extends TypedScale {
  lazy val pythagoreanScale = ScaleBuilderPythagFull(apply(0).hz).build()

  def closestToPythagIndex(index: Int): Int = {
    val hz = apply(index).hz
    ((pythagoreanScale.intervals map {
      i => Math.abs(i.hz - hz)
    } zipWithIndex).
      minBy(_._1))._2
  }

  def closestToPythagIndices: Seq[Int] =
    0 until size map (closestToPythagIndex)
}
