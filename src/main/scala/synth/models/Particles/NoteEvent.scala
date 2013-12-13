package synth.models.Particles

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 12/10/13
 * Time: 5:09 PM
 * To change this template use File | Settings | File Templates.
 */

sealed abstract class NoteEvent extends DES.Event[NoteEvent] {

  def note: Option[NoteEvent.Note]

  def time: Int

  def priority: Int
}

object NoteEvent {

  type Note = String

  val BIRTH_PRIORITY = 1
  val DEATH_PRIORITY = 2
  val DECAY_PRIORITY = 3

  case class Birth(override val note: Option[Note], override val time: Int, lifespan: Int) extends NoteEvent {
    def priority: Int = NoteEvent.BIRTH_PRIORITY
  }

  case class Death(override val note: Option[Note], override val time: Int, lifespan: Int) extends NoteEvent {
    def priority: Int = NoteEvent.DEATH_PRIORITY
  }

  case class Decay(override val time: Int) extends NoteEvent {
    def priority: Int = NoteEvent.DECAY_PRIORITY

    def note: Option[NoteEvent.Note] = None
  }

  object NoteLifeProcessor extends DES.EventProcessor[NoteEvent] {
    def handler(e: NoteEvent, des: DES[NoteEvent]): DES[NoteEvent] = e match {
      case Birth(note, time, life) => des addEvent Death(note, time + life, life)
      case Death(note, time, life) => des addEvent Decay(time)
      case Decay(time) => des
      case _ => throw sys.error("unknown event type " + e)
    }

    def process: (NoteEvent, DES[NoteEvent]) => DES[NoteEvent] = handler
  }

}

