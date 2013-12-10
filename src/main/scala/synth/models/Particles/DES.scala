package synth.models.Particles

import scala.collection.immutable.SortedSet

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 12/10/13
 * Time: 3:27 PM
 * To change this template use File | Settings | File Templates.
 */

object DES {
  type Action[C <: Event[C]] = DES[C] => DES[C]

  trait Event[C <: Event[C]] {
    self: C =>
    def action: Action[C]
  }

}

class DES[E <: DES.Event[E]](private val queue: SortedSet[E]) {

  def peek: E = queue.firstKey

  def next(): DES[E] = peek.action(new DES(queue - peek))

  def addEvent(e: E): DES[E] = new DES(queue + e)

  def play: DES[E] = {
    var s = this
    while (!s.queue.isEmpty) {
      s = s.next()
    }
    s
  }
}
