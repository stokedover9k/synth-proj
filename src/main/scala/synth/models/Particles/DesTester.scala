package synth.models.Particles

import scala.collection.immutable.SortedSet

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 12/10/13
 * Time: 4:49 PM
 * To change this template use File | Settings | File Templates.
 */
object DesTester extends App {

  import DES._

  class Ev(t: Int, p: Int, m: String) extends Event[Ev] {
    def time = t

    def prio = p

    def msg = m

    def action: (DES[Ev]) => DES[Ev] =
      sim => sim
  }

  case class Birth(override val time: Int) extends Ev(time, 1, "birth")

  case class Death(override val time: Int) extends Ev(time, 2, "death")

  implicit val EOrder: Ordering[Ev] = Ordering[(Int, Int, String)].on[Ev](e => (e.time, e.prio, e.msg))

  implicit val printer = (ev: Ev, des: DES[Ev]) => {
    println(ev)
    des
  }

  val sim = (1 to 10).foldLeft(new DES[Ev](SortedSet[Ev]())) {
    case (s: DES[Ev], i: Int) => {
      if (Math.random() < .5)
        s.addEvent(Birth(i / 2))
      else
        s.addEvent(Death(i / 2))
    }
  }

  sim.play
}
