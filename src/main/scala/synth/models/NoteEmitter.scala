package synth.models

import util.{Counters, Counter, CounterMap}
import synth.Series.Interval
import util.expr.Fraction
import synth.scales.{ScaleBuilderZarlino, TypedScale, Scale}
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

object DemoChordPlayer {

  lazy val toneComponents = synth.sounds.ChordPlayer.toneComponents

  val SAMPLE_RATE = synth.sounds.SAMPLE_RATE
  lazy val outFormat = new AudioFormat(SAMPLE_RATE, 8, 1, true, true)
  lazy val line = synth.sounds.getLine(outFormat)

  lazy val player = SoundStream.streamPlayerFor(line)(SAMPLE_RATE.toInt / 8)

  def demoPlay(scale: TypedScale, song: List[String]): Unit = {
    val s: List[(String, Float)] = song map (n => (n, .5f))
    lazy val chordFrames = s map {
      case (note: String, duration: Float) =>
        ComplexChord(Seq(ComplexTone(scale(note).hz, toneComponents))) -> (duration * SAMPLE_RATE).toInt
    }
    demoPlay(chordFrames)
  }

  def demoPlayChords(scale: TypedScale, song: List[Seq[String]]): Unit = {
    def tones(notes: Seq[String]) = notes map {
      note => ComplexTone(scale(note).hz, toneComponents)
    }
    def chords = song map {
      chord => ComplexChord(tones(chord)) -> (.5f * SAMPLE_RATE).toInt
    }
    demoPlay(chords)
  }

  def demoPlay(chordFrames: List[(ComplexChord, Int)]): Unit = {
    def songStream =
      SoundStream.getSampleStream(chordFrames) map {
        Formats.floatConverter(line.getFormat)
      } flatten

    player(new AtomicReference(songStream), -1)
    line.drain
  }
}
