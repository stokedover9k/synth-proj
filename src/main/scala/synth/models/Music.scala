package synth.models

import synth.scales.{ScaleBuilderZarlino, Scale}
import synth.Series
import util.expr.Fraction

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 12/2/13
 * Time: 4:49 PM
 * To change this template use File | Settings | File Templates.
 */

class Music {

}

abstract class MState[OBSERVATION] {

  def possibleEvents: Stream[OBSERVATION]

  def prob(o: OBSERVATION): Double

  def prob(o1: OBSERVATION, o2: OBSERVATION): Double

  /*
   * probability of o1 given o0
   */
  def condProb(o0: OBSERVATION)(o1: OBSERVATION) = prob(o0, o1) / prob(o0)

  def emitEvent(current: OBSERVATION): OBSERVATION = {
    var rand = Math.random()
    var events = possibleEvents

    def emit: OBSERVATION = {
      val p = condProb(current)(events.head)
      if (events.tail.isEmpty)
        events.head
      else if (p < rand) {
        rand -= p
        events = events.tail
        emit
      }
      else
        events.head
    }
    emit
  }

  def eventStream(start: OBSERVATION): Stream[OBSERVATION] = {
    val next = emitEvent(start)
    next #:: eventStream(next)
  }
}

abstract class MDelta[OBSERVATION] {

  def apply(s: MState[OBSERVATION]): MState[OBSERVATION]
}

abstract class IntervalEmitter extends MState[Series.Interval] {

}

class ScaleState(scale: Scale) extends IntervalEmitter {

  import Series.Interval

  def possibleEvents = scale.intervals.toStream

  def prob(interval: Interval): Double = {
    assert(scale.intervals.exists {
      i => i.hzFactor.toFloat == interval.hzFactor.toFloat
    })
    1d / scale.size
  }

  def prob(o1: Interval, o2: Interval): Double =
    p(o1, o2) * 2 / Z

  private def p(o1: Interval, o2: Interval): Double = {
    o1.hzFactor.div(o2.hzFactor) match {
      case frac: Fraction => {
        Math.abs(frac.num.toFloat - frac.denom.toFloat) / (frac.num.toFloat + frac.denom.toFloat)
      }
      case f => throw sys.error("expecting a fractional interval ratio but found '%s'".format(f.toString))
    }
  }

  lazy val Z: Double = {
    var sum: Double = 0
    for (i <- possibleEvents)
      for (j <- possibleEvents)
        if (i.hz != j.hz)
          sum = sum + p(i, j)
    sum
  }
}

object ScaleState {

  def main(args: Array[String]): Unit = {

    val scale = ScaleBuilderZarlino(528).build

    def name(interval: Series.Interval): String = {
      val index = scale.intervals.zipWithIndex.find(i => i._1.hz == interval.hz).get._2
      scale.allNames(index)
    }

    val state = new ScaleState(scale)

    println("interval probabilities")
    for (s <- state.possibleEvents)
      println(s.hzFactor + " " + state.prob(s))

    for (s1 <- state.possibleEvents)
      for (s2 <- state.possibleEvents)
        println(s1.hzFactor + " " + s2.hzFactor + " " + state.prob(s1, s2) + " " + state.condProb(s1)(s2))

    val first = scale(0)
    val events = state.eventStream(first) take 20 toList
    val eventPairs = (first :: events) zip events

    eventPairs foreach {
      case (i1, i2) =>
        println("%s => %s    %.5f".format(name(i1), name(i2), state.condProb(i1)(i2)))
    }
  }
}



