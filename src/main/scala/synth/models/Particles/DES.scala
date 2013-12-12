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
  }

  private var __NEVER_CALLED_PROCESSOR__ = true

  def doNothingProcessor[E <: Event[E]] = (e: E, des: DES[E]) => {
    if (__NEVER_CALLED_PROCESSOR__) {
      System.err.println("[warning] Using default doNothingProcessor to process DES events.")
      __NEVER_CALLED_PROCESSOR__ = false
    }
    des
  }

  trait EventProcessor[E <: Event[E]] {
    def process: (E, DES[E]) => DES[E]
  }

  object EventProcessor {

    class ChainableEventProcessor[E <: DES.Event[E]](val first: EventProcessor[E]) {
      def andThen(next: EventProcessor[E]): EventProcessor[E] = new EventProcessor[E] {
        def process: (E, DES[E]) => DES[E] = (e, des) => {
          val firstRes = first.process(e, des)
          next.process(e, firstRes)
        }
      }
    }

    implicit def chainEventProcessors[E <: DES.Event[E]](p: EventProcessor[E]) = new ChainableEventProcessor[E](p)
  }

  implicit def ProcessorToFunction[E <: Event[E]](implicit proc: EventProcessor[E]): (E, DES[E]) => DES[E] = proc.process

}

class DES[E <: DES.Event[E]](private val queue: SortedSet[E]) {

  type EVENT_PROCESSOR = (E, DES[E]) => DES[E]

  def peek: E = queue.firstKey

  def size: Int = queue.size

  def next(implicit process: EVENT_PROCESSOR): DES[E] =
    process(peek, new DES(queue - peek))

  def addEvent(e: E): DES[E] = new DES(queue + e)

  def play(implicit process: EVENT_PROCESSOR): DES[E] = {
    var s = this
    while (!s.queue.isEmpty)
      s = s.next(process)
    s
  }
}
