package synth.models.Particles

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 12/10/13
 * Time: 5:09 PM
 * To change this template use File | Settings | File Templates.
 */

sealed abstract class NoteEvent extends DES.Event[NoteEvent] {

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

  object NoteLifeProcessor extends DES.EventProcessor[NoteEvent] {
    def handler(e: NoteEvent, des: DES[NoteEvent]): DES[NoteEvent] = e match {
      case Birth(note, time, life) => des addEvent Death(note, time + life, life)
      case Death(note, time, life) => des addEvent Decay(note, time, life)
      case Decay(note, time, life) => des
      case _ => throw sys.error("unknown event type " + e)
    }

    def process: (NoteEvent, DES[NoteEvent]) => DES[NoteEvent] = handler
  }

}

