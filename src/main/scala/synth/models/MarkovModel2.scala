package synth.models

import util.{CounterMap, Counters, Counter}
import synth.scales.{ScaleBuilderRameau, TypedScale}

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 12/5/13
 * Time: 4:12 PM
 * To change this template use File | Settings | File Templates.
 */

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

  def transitionCosts: Counter[Traversable[T]] = {
    val counter = new Counter[Traversable[T]]()
    transitions foreach {
      t => counter.incrementCount(t, (t map elementCost).sum)
    }
    counter
  }
}

abstract class FullCombinationState[T] extends CombinationState[T] {

  def elements: Seq[T]

  def transitions: TraversableOnce[Traversable[T]] = {
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
                  )
  extends FullCombinationState[String] {

  def elementCost(t: String): Double =
    (myNotes map (n => dissonance(t, n))).sum

  def apply(trans: Traversable[String]): ChordState = new ChordState(elements, trans, dissonance)
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
                        , val heat: Heat[String]
                        )
  extends ChordState(elements, myNotes, dissonance) {

  override def elementCost(t: String): Double =
    (myNotes map (n => dissonance(t, n) * heat(n))).sum + 1

  override def apply(trans: Traversable[String]): HeatedChordState = {
    trans foreach (t => heat.heatUp(t, 1))
    heat.cool(heat.totalHeat)
    new HeatedChordState(elements, trans, dissonance, heat)
  }

  override def toString: String = elements.toString
}

object HeatedChordState {

  def apply(scale: TypedScale, dissonance: (String, String) => Double): HeatedChordState = {
    val heat = new Heat[String]
    uniqueNotes(scale) foreach (n => heat.heatUp(n))
    new HeatedChordState(uniqueNotes(scale), Seq[String](), dissonance, heat)
  }

  def uniqueNotes(scale: TypedScale) =
    scale.allNames.groupBy(n => n).map(_._1).toList

  def dissonanceFunction(scale: TypedScale): (String, String) => Double = {
    val notes = uniqueNotes(scale)

    val counts = new CounterMap[String, String]()

    for (n1 <- notes)
      for (n2 <- notes) {
        val dis = SuperParticularDissonance(scale(n1))(scale(n2))
        counts.incrementCount(n1, n2, dis)
      }

    (a: String, b: String) => counts.getCount(a, b)
  }

  def main(args: Array[String]): Unit = {

    val scale = ScaleBuilderRameau(528).build
    val dissonance = dissonanceFunction(scale)
    val state = apply(scale, dissonance)

    MarkovModelTester.runRounds(state, 5)
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
