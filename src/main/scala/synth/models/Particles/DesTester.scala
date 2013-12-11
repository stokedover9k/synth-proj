package synth.models.Particles

import scala.collection.immutable.SortedSet
import synth.models.Particles.DES.EventProcessor

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

object DesNoteEventTester extends App {

  object NoteLifeProcessor extends EventProcessor[NoteEvent] {
    def handler(e: NoteEvent, des: DES[NoteEvent]): DES[NoteEvent] = e match {
      case Birth(note, time, life) => des addEvent Death(note, time + life, life)
      case Death(note, time, life) => des addEvent Decay(note, time, life)
      case Decay(note, time, life) => des
      case _ => throw sys.error("unknown event type " + e)
    }

    def process: (NoteEvent, DES[NoteEvent]) => DES[NoteEvent] = handler
  }

  object NoteLifePrinter extends EventProcessor[NoteEvent] {
    def toString(e: NoteEvent): String =
      "%s [born %d died %d] (t %d p %d)".format(e.note, e.bornAt, e.diedAt, e.time, e.priority)

    def handler(e: NoteEvent): Unit = e match {
      case Birth(note, time, life) => println("+ " + toString(e))
      case Death(note, time, life) => println("- " + toString(e))
      case Decay(note, born, life) => println("% " + toString(e))
      case _ => throw sys.error("unknown event type " + e)
    }

    def process: (NoteEvent, DES[NoteEvent]) => DES[NoteEvent] = (e, des) => {
      handler(e)
      des
    }
  }

  implicit val proc = NoteLifePrinter andThen NoteLifeProcessor


  implicit val EOrder: Ordering[NoteEvent] = Ordering[(Int, Int)].on[NoteEvent](e => (e.time, e.priority))

  val notes = "A B C D E F G".split("\\s").toIndexedSeq

  val des = (1 to 10).foldLeft(new DES[NoteEvent](SortedSet())) {
    case (d, i) => {
      val note = notes((Math.random() * notes.size).toInt)
      val bornAt = (Math.random() * 10).toInt
      val lifespan = (Math.random() * 10).toInt
      d addEvent Birth(note, bornAt, lifespan)
    }
  }

  des.play
}
