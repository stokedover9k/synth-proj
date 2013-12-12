package synth.models

import util.{CounterMap, Counters, Counter}
import synth.scales.{ScaleBuilderRameau, TypedScale}
import synth.Series.Interval
import util.expr.Fraction

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 12/5/13
 * Time: 4:12 PM
 * To change this template use File | Settings | File Templates.
 */

object SuperParticularDissonance2 extends Dissonance {
  def apply(i1: Interval)(i2: Interval): Double =
    i1.hzFactor.div(i2.hzFactor) match {
      case frac: Fraction => {
        val n = frac.num.toFloat
        val d = frac.denom.toFloat
        Math.log(Math.pow((n + d) / (1 + Math.abs(n - d)), 1))
      }
      case f =>
        throw sys.error("expecting a fractional interval ratio but found '%s'".format(f.toString))
    }
}

abstract class MarkovModel2[D, S <: State[D]] {


}

object MarkovModel2 {

  def getTransition[D](state: State[D]): D =
    Counters.sample(state.transitionProbs)

}

trait State[T] {

  def apply(trans: T): State[T]

  def transitions: TraversableOnce[T]

  def transitionProbs: Counter[T]

  def prob(trans: T): Double = transitionProbs.getCount(trans)
}

trait HasTransitionCosts[T] extends State[T] {

  def cost(trans: T): Double = transitionCosts.getCount(trans)

  def transitionCosts: Counter[T]

  override lazy val transitionProbs: Counter[T] = {
    val counter = new Counter[T]()
    val itr = transitionCosts.getEntrySet().iterator()
    while (itr.hasNext) {
      val entry = itr.next
      counter.incrementCount(entry.getKey, 1d / entry.getValue)
    }
    counter.normalize
    counter
  }
}

abstract class CombinationState[T] extends HasTransitionCosts[Traversable[T]] {

  def elementCost(t: T): Double

  def combinationCost: Traversable[T] => Double

  def transitionCosts: Counter[Traversable[T]] = {
    val counter = new Counter[Traversable[T]]()
    transitions foreach {
      t => counter.incrementCount(t, (t map elementCost).sum * combinationCost(t))
    }
    counter
  }
}

abstract class FullCombinationState[T] extends CombinationState[T] {

  def elements: Seq[T]

  def transitions: TraversableOnce[Traversable[T]] = FullCombinationState.allCombinations(elements)
}

object FullCombinationState {
  def allCombinations[T](elements: Seq[T]): TraversableOnce[Traversable[T]] = {
    def loop(n: Int): Stream[Seq[T]] = {
      if (n <= 0) Stream()
      else if (n == 1) elements.combinations(n).toStream
      else elements.combinations(n).toStream #::: loop(n - 1)
    }
    loop(elements.size)
  }
}

class ChordState(
                  override val elements: Seq[String]
                  , val myNotes: Traversable[String]
                  , val dissonance: (String, String) => Double
                  , val chordCosts: Traversable[String] => Double
                  )
  extends FullCombinationState[String] {

  def elementCost(t: String): Double = (myNotes map (n => dissonance(t, n))).sum

  def apply(trans: Traversable[String]): ChordState = new ChordState(elements, trans, dissonance, chordCosts)

  def combinationCost: (Traversable[String]) => Double = chordCosts
}

class Heat[T] {
  private val heat = new Counter[T]()

  def heatUp(element: T, v: Double = 1): Unit = heat.incrementCount(element, v)

  def cool(v: Double = 2): Unit = heat.scale(1d / v)

  def totalHeat: Double = heat.totalCount()

  def apply(element: T): Double = heat.getCount(element)

  def getCounter = heat
}

class HeatedChordState(
                        override val elements: Seq[String]
                        , override val myNotes: Traversable[String]
                        , override val dissonance: (String, String) => Double
                        , override val chordCosts: Traversable[String] => Double
                        , val heat: Heat[String]
                        )
  extends ChordState(elements, myNotes, dissonance, chordCosts) {

  override def elementCost(t: String): Double =
  //    (myNotes map (n => dissonance(t, n) * heat(n))).sum + 1
    (elements map (n => dissonance(t, n) * heat(n))).sum + 1

  override def apply(trans: Traversable[String]): HeatedChordState = {
    trans foreach (t => heat.heatUp(t, 1))
    heat.cool(heat.totalHeat)
    new HeatedChordState(elements, trans, dissonance, chordCosts, heat)
  }

  override def toString: String = myNotes.toString
}

object HeatedChordState {

  implicit val allowAllChords: Traversable[String] => Boolean = chord => true

  def getChordCosts(notes: Seq[String], dissonance: (String, String) => Double)(implicit chordFilter: Traversable[String] => Boolean): Counter[Traversable[String]] = {
    val counter = new Counter[Traversable[String]]()
    for (chord <- FullCombinationState.allCombinations(notes).filter(chordFilter)) {
      var sum: Double = 0
      for (n1 <- chord)
        for (n2 <- chord)
          sum += dissonance(n1, n2)
      counter.incrementCount(chord, sum)
    }
    counter
  }

  def apply(scale: TypedScale, dissonance: (String, String) => Double)(implicit chordFilter: Traversable[String] => Boolean): HeatedChordState = {
    val heat = new Heat[String]
    uniqueNotes(scale) foreach (n => heat.heatUp(n))

    val chordCosts = getChordCosts(uniqueNotes(scale), dissonance)(chordFilter)

    new HeatedChordState(uniqueNotes(scale), Seq[String](), dissonance, chordCosts.getCount, heat)
  }

  def uniqueNotes(scale: TypedScale) = scale.allNames.groupBy(n => n).map(_._1).toList

  def dissonanceFunction(scale: TypedScale): (String, String) => Double = {
    val notes = uniqueNotes(scale)
    val counts = new CounterMap[String, String]()

    for (n1 <- notes)
      for (n2 <- notes) {
        val dis = SuperParticularDissonance2(scale(n1))(scale(n2))
        counts.incrementCount(n1, n2, dis)
      }

    (a: String, b: String) => counts.getCount(a, b)
  }

  def main(args: Array[String]): Unit = {

    val scale = ScaleBuilderRameau(528).build
    val dissonance = dissonanceFunction(scale)
    val state = apply(scale, dissonance)

    //    MarkovModelTester.runRounds(state, 5)
    val states = MarkovModelTester.chordStateStream(state) take 20

    val song = states.toList map (_.myNotes.toSeq)
    song foreach println

    DemoChordPlayer.demoPlayChords(scale, song)
  }
}


object MarkovModelTester extends App {

  case class S(strings: Seq[String]) extends State[String] {

    lazy val transitionProbs: Counter[String] = {
      val counter = new Counter[String]()
      strings foreach (str => counter.incrementCount(str, 1))
      counter.normalize()
      counter
    }

    lazy val transitions: TraversableOnce[String] = transitionProbs.keySet.toArray(Array[String]())

    def apply(delta: String) = this
  }

  case class City(name: String) extends HasTransitionCosts[String] {
    override def cost(trans: String): Double = (name, trans) match {
      case ("NY", "Boston") | ("Boston", "NY") => 1
      case ("NY", "Atlanta") | ("Atlanta", "NY") => 2
      case ("Boston", "Atlanta") | ("Atlanta", "Boston") => 3
      case (a: String, b: String) => if (a == b) .5 else sys.error("don't know these cities " + a + " " + b)
    }

    def transitionCosts: Counter[String] = {
      val costs = new Counter[String]()
      transitions foreach (key => costs.incrementCount(key, cost(key)))
      costs
    }

    val transitions: TraversableOnce[String] = Seq("Atlanta", "Boston", "NY")

    def apply(trans: String) = City(trans)
  }

  def stateStream[T](begin: State[T]): Stream[State[T]] = {
    def loop(s: State[T]): Stream[State[T]] = {
      val delta = MarkovModel2.getTransition(s)
      val prob = s.prob(delta)
      val next = s(delta)
      next #:: loop(next)
    }
    begin #:: loop(begin)
  }

  def chordStateStream[T](begin: ChordState): Stream[ChordState] = {
    def loop(s: ChordState): Stream[ChordState] = {
      val delta = MarkovModel2.getTransition(s)
      val prob = s.prob(delta)
      val next = s(delta)
      next #:: loop(next)
    }
    begin #:: loop(begin)
  }

  def runRounds[T](begin: State[T], num: Int): Unit = {
    if (num > 0) {
      val delta = MarkovModel2.getTransition(begin)
      val prob = begin.prob(delta)
      val next = begin(delta)
      println(begin, "--", delta, "-->", next, prob)
      runRounds(next, num - 1)
    }
  }

  runRounds(S(Seq("a", "b", "c")), 5)

  runRounds(City("NY"), 10)

}
