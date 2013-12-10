package synth.models.Particles

import scala.collection.immutable.SortedSet

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 12/9/13
 * Time: 7:50 PM
 * To change this template use File | Settings | File Templates.
 */

//trait Event[C <: Event[C]] {
//  self: C =>
//  def andThen: C
//}
//
//case class Birth extends Event[Birth] {
//  def andThen: Birth = this
//}
//
//class DES[E <: Event] (queue: List[E]) {
//  def addEvent(e: E): DES[E] = new DES(e :: queue)
//}

object OldDES {

  type Action = () => Unit

  trait Event[C <: Event[C]] {

    self: C =>

    def time: Int

    def priority: Int

    def action: Action

    def andThen[T <: Event[T]](other: Event[T]): C

  }

  def eventOrdering[C <: Event[C]] = Ordering[(Int, Int)].on[C](e => e.time -> e.priority)

}

class OldDES[E <: OldDES.Event[E]](queue: SortedSet[E] = SortedSet[E]()(OldDES.eventOrdering[E])) {

  /*
   * If the event already belongs to the queue (according to queue's ordering),
   * it is combined with the existing event and added.
   */
  def addEvent(event: E): OldDES[E] = {
    val ordering = queue.ordering
    val inserting =
      if (queue.contains(event))
        event andThen queue.find(ordering.equiv(event, _)).get
      else
        event
    new OldDES[E](queue + inserting)
  }

  def peek: E = queue.firstKey

  def next: OldDES[E] = new OldDES(queue - peek)

  def time: Int = peek.time

  def play(N: Int = -1): OldDES[E] = {
    val n = if( N < 0 ) queue.size else N
    queue take n foreach (_.action())
    new OldDES(queue drop n)
  }
}

object OldDESTester extends App {

  import OldDES.Event
  import OldDES.Action

  class LifetimeEvent(override val time: Int, override val priority: Int, override val action: Action)
    extends Event[LifetimeEvent] {

    def andThen[T <: Event[T]](other: Event[T]): LifetimeEvent = new LifetimeEvent(
      time,
      priority,
      () => {
        println("---begin")
        action()
        other.action()
        println("---end")
      }
    )
  }

  case class Birth(t: Int) extends LifetimeEvent(t, 1, () => println("birth", t))

  case class Death(t: Int) extends LifetimeEvent(t, 3, () => println("death", t))

  val des = (1 to 10).foldLeft(new OldDES[LifetimeEvent]) {
    case (des: OldDES[LifetimeEvent], i: Int) => {
      if (Math.random() < .5)
        des.addEvent(new Birth(i / 2))
      else
        des.addEvent(new Death(i / 2))
    }
  }

  des.play()

}
