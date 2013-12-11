package synth.models.Particles

import scala.collection.mutable
import synth.models.Particles.DES.EventProcessor
import synth.models.Particles.NoteEvent.{Birth, Death, Decay}
import synth.models.Heat


/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 12/11/13
 * Time: 1:24 PM
 * To change this template use File | Settings | File Templates.
 */

class NoteSystem {

  import NoteEvent.Note

  val playing: mutable.Set[Note] = mutable.Set[Note]()
  val stopping: mutable.Set[Note] = mutable.Set[Note]()

  override def toString = playing.toString + " " + stopping.toString

  def emit(note: Note): Unit =
    playing += note

  def stop(note: Note): Unit = {
    playing -= note
    stopping += note
  }

  def remove(note: Note): Unit = {
    stopping -= note
  }

  def flushStopped: Unit =
    stopping.clear

}

abstract class NoteSystemProcessor extends EventProcessor[NoteEvent] {
  def noteSystem: NoteSystem

  def process: (NoteEvent, DES[NoteEvent]) => DES[NoteEvent] = (e, des) => e match {
    case e: Birth => processBirth(e, des)
    case e: Death => processDeath(e, des)
    case e: Decay => processDecay(e, des)
    case _ => des
  }

  def processBirth(e: Birth, des: DES[NoteEvent]): DES[NoteEvent]

  def processDeath(e: Death, des: DES[NoteEvent]): DES[NoteEvent]

  def processDecay(e: Decay, des: DES[NoteEvent]): DES[NoteEvent]
}

abstract class HeatedNoteSystemProcessor extends NoteSystemProcessor {

  import NoteEvent.Note

  def noteHeat: Heat[Note]

  def noteSystem: NoteSystem

  def processBirth(e: Birth, des: DES[NoteEvent]): DES[NoteEvent] = {
    noteSystem.emit(e.note.get)
    noteHeat.heatUp(e.note.get, 1)
    des addEvent Death(e.note, e.time + e.lifespan, e.lifespan)
  }

  def processDeath(e: Death, des: DES[NoteEvent]): DES[NoteEvent] = {
    noteSystem.stop(e.note.get)
    des addEvent Decay(e.time)
  }

  def emit(playing: Traversable[Note], stopping: Traversable[Note], heat: Heat[Note], time: Int): Traversable[Birth]

  def processDecay(e: Decay, des: DES[NoteEvent]): DES[NoteEvent] = {
    noteHeat.cool(noteHeat.totalHeat)
    val newDes =
      emit(noteSystem.playing, noteSystem.stopping, noteHeat, e.time).foldLeft(des) {
        case (d, b) => d addEvent b
      }
    noteSystem.flushStopped
    newDes
  }
}
