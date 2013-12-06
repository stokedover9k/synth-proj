package synth.models

import util.{Counters, Counter}

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
