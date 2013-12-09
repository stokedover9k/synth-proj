package synth.models

import util.{Counters, Counter, CounterMap}
import synth.Series.Interval
import util.expr.Fraction
import synth.scales._
import synth.sounds.{ComplexTone, ComplexChord}
import synth.sounds.util.{SoundStream, Formats}
import javax.sound.sampled.AudioFormat
import java.util.concurrent.atomic.AtomicReference

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 12/3/13
 * Time: 8:14 PM
 * To change this template use File | Settings | File Templates.
 */

trait MagicPoints[T] {

  def magicPoints: T => T => Double
}

abstract class FavoredState[T]
  extends MState[T] with MagicPoints[T] {

  private val eventDistrib = new Counter[T]()

  private val condDistrib = {
    val d = new CounterMap[T, T]()
    for (i <- possibleEvents)
      for (j <- possibleEvents)
        d.setCount(i, j, magicPoints(i)(j))
    val key = d.keySet.iterator
    while (key.hasNext)
      eventDistrib.incrementAll(d.getCounter(key.next))
    eventDistrib.normalize()
    Counters.conditionalNormalize(d)
  }

  def prob(o1: T, o2: T): Double = condDistrib.getCount(o1, o2) * eventDistrib.getCount(o1)

  override def condProb(o1: T)(o2: T): Double = condDistrib.getCount(o1, o2)
}

abstract class Dissonance {
  def apply(i1: Interval)(i2: Interval): Double
}

object SuperParticularDissonance extends Dissonance {
  def apply(i1: Interval)(i2: Interval): Double =
    i1.hzFactor.div(i2.hzFactor) match {
      case frac: Fraction =>
        if (frac.toFloat == 1)
          1
        else
          Math.pow(1 + Math.abs(frac.num.toFloat - frac.denom.toFloat) / (frac.num.toFloat + frac.denom.toFloat), 2)
      case f =>
        throw sys.error("expecting a fractional interval ratio but found '%s'".format(f.toString))
    }
}

class NoteEmitter(scale: TypedScale,
                  dissonance: Dissonance)
  extends FavoredState[String] {

  def possibleEvents: Stream[String] = scale.allNames.toStream

  def prob(o: String): Double = 1d / scale.size

  def magicPoints: (String) => (String) => Double =
    n1 => n2 => 1d / dissonance.apply(scale(n1))(scale(n2))
}

object NoteEmitter {

  def main(args: Array[String]): Unit = {

    val scale = ScaleBuilderZarlino(528).build

    val state = new NoteEmitter(scale, SuperParticularDissonance)

    println("===== interval probabilities =====")
    for (s <- state.possibleEvents)
      println("%s  %.5f".format(s, state.prob(s)))

    for (s1 <- state.possibleEvents)
      for (s2 <- state.possibleEvents)
        println("%s %s %.5f %.5f".format(s1, s2, state.prob(s1, s2), state.condProb(s1)(s2)))

    val first = scale.allNames(0)
    val events = state.eventStream(first) take 20 toList
    val eventPairs = (first :: events) zip events

    eventPairs foreach {
      case (n1, n2) =>
        println("%s => %s    %.5f".format(n1, n2, state.condProb(n1)(n2)))
    }

    ////////////////////////////////////////////////////

    DemoChordPlayer.demoPlay(scale, events)
  }
}

class ChordEmitter(scale: TypedScale,
                   dissonance: Dissonance)
  extends FavoredState[Seq[String]] {

  lazy val noteEmitter = new NoteEmitter(scale, dissonance)

  private lazy val names = scale.allNames.groupBy(s => s).keys.toSeq

  def possibleEvents: Stream[Seq[String]] = {
    def loop(n: Int): Stream[Seq[String]] = {
      if (n <= 0)
        Stream()
      else
        names.combinations(n).toStream #::: loop(n - 1)
    }
    loop(names.size)
  }

  def magicPoints: (Seq[String]) => (Seq[String]) => Double =
    ch1 => ch2 => {
      var sum = 0d
      for (s1 <- ch1)
        for (s2 <- ch2)
          sum += noteEmitter.magicPoints(s1)(s2)
      sum
    }

  lazy val Z: Double = {
    var sum = 0d
    for (ch <- possibleEvents)
      sum += 1d / magicPoints(ch)(ch)
    sum
  }

  def prob(o: Seq[String]): Double = {
    1d / magicPoints(o)(o) / Z
  }
}

object ChordEmitter {

  def main(args: Array[String]): Unit = {

    val scale = ScaleBuilderZarlino(528).build

    val state = new ChordEmitter(scale, SuperParticularDissonance)

    println("===== interval probabilities =====")
    println("(# %s)".format(state.possibleEvents.size))
    for (s <- state.possibleEvents)
      println("%s  %.5f".format(s, state.prob(s)))

    val first = Seq(scale.allNames(0))
    val events = state.eventStream(first) take 20 toList
    val eventPairs = (first :: events) zip events

    eventPairs foreach {
      case (n1, n2) =>
        println("%s => %s    %.5f".format(n1, n2, state.condProb(n1)(n2)))
    }

    ////////////////////////////////////////////////////

    DemoChordPlayer.demoPlayChords(scale, events)
  }
}

object QuizStuff {
  val toneComponents = Seq(
    ComplexTone.Component(1f, 1f)
  )

  val scale = ScaleBuilderPythagHepto(400).build

  val first = ComplexTone(scale(IntervalType.First).hz, toneComponents)

  val tone3 = ComplexTone(scale(IntervalType.Major3).hz, toneComponents)
  val pure3 = ComplexTone(400f * 5 / 4, toneComponents)

  def play(chords: List[(ComplexChord, Int)]): Unit = {
    val chordFrames = chords map {
      case (chord: ComplexChord, seconds: Int) => (chord, (seconds * DemoChordPlayer.SAMPLE_RATE).toInt)
    }
    DemoChordPlayer.demoPlay(chordFrames)
  }
}

object Quiz1 extends App {
  import QuizStuff._

  println(scale(IntervalType.Major3).hz)
  println(400f * 5 / 4)

  val chord1 = ComplexChord(Seq(tone3))
  val chord2 = ComplexChord(Seq(tone3, pure3))

  val chords = List((chord1, 3), (chord2, 3))

  play(chords)
}

object Quiz2 extends App {
  import QuizStuff._

  val chord1 = ComplexChord(Seq(first, tone3))
  val chord2 = ComplexChord(Seq(first, pure3))

  val chords = List((chord1, 3), (chord2, 3))

  play(chords)
}
