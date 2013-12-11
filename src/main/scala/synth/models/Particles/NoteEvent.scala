package synth.models.Particles

import scala.collection.immutable.SortedSet

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 12/10/13
 * Time: 5:09 PM
 * To change this template use File | Settings | File Templates.
 */

abstract class NoteEvent extends DES.Event[NoteEvent] {

  def note: String

  def time: Int

  def priority: Int

  def lifespan: Int

  def bornAt: Int

  def diedAt: Int
}

object NoteEvent {
  val BIRTH_PRIORITY = 1
  val DEATH_PRIORITY = 2
  val DECAY_PRIORITY = 3
}

case class Birth(override val note: String, override val time: Int, override val lifespan: Int) extends NoteEvent {
  def priority: Int = NoteEvent.BIRTH_PRIORITY

  def bornAt: Int = time

  def diedAt: Int = time + lifespan
}

case class Death(override val note: String, override val time: Int, override val lifespan: Int) extends NoteEvent {
  def priority: Int = NoteEvent.DEATH_PRIORITY

  def bornAt: Int = time - lifespan

  def diedAt: Int = time
}

case class Decay(override val note: String, override val time: Int, override val lifespan: Int) extends NoteEvent {
  def priority: Int = NoteEvent.DECAY_PRIORITY

  def bornAt: Int = time - lifespan

  def diedAt: Int = time
}

object DesNoteEventTester extends App {

  def toString(e: NoteEvent): String =
    "%s [born %d died %d] (t %d p %d)".format(e.note, e.bornAt, e.diedAt, e.time, e.priority)

  implicit def eventPrinter(e: NoteEvent, des: DES[NoteEvent]): DES[NoteEvent] = e match {
    case Birth(note, time, life) => {
      println("+ " + toString(e))
      des.addEvent(Death(note, time + life, life))
    }
    case Death(note, time, life) => {
      println("- " + toString(e))
      des addEvent Decay(note, time, life)
    }
    case Decay(note, born, life) => {
      println("% " + toString(e))
      des
    }
    case _ => throw sys.error("unknown event type " + e)
  }

  implicit val EOrder: Ordering[NoteEvent] = Ordering[(Int, Int)].on[NoteEvent](e => (e.time, e.priority))

  val notes = "A B C D E F G".split("\\s").toIndexedSeq

  val des = (1 to 10).foldLeft(new DES[NoteEvent](SortedSet())) {
    case (d, i) => d.addEvent(Birth(notes((Math.random() * notes.size).toInt), (Math.random() * 10).toInt, (Math.random() * 10).toInt))
  }

  des.play
}
