package synth.models

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 12/2/13
 * Time: 4:51 PM
 * To change this template use File | Settings | File Templates.
 */

abstract class MarkovModel {

  /*
   * State of the model
   */
  abstract class State {
    def transitions: TraversableOnce[Delta]

    def prob(d: Delta): Double
  }

  /*
   * Transition between states
   */
  abstract class Delta {
    def apply(s: State): State
  }

  /*
   * probability of a given state
   */
  def prob(s: State): Double

  /*
   * probability of transitioning to s1 from s0
   */
  def prob(s0: State, s1: State): Double

  /*
   * probability of transitioning from a state by a the given delta
   */
  def prob(s: State, d: Delta): Double = prob(s, d(s))
}
