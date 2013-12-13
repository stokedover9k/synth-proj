package synth.models.Particles

import scala.collection.immutable.SortedSet
import synth.models.Particles.DES.EventProcessor
import synth.models.DemoChordPlayer
import synth.scales.{ScaleBuilderEvenTempFull, ScaleBuilderRameau}

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

  import NoteEvent._


  object NoteLifePrinter extends EventProcessor[NoteEvent] {
    private def stringPattern = "%5d: %s %-2s %5d %5d %5d"

    def handler(e: NoteEvent): Unit = e match {
      case Birth(note, time, life) => println(stringPattern.format(time, "+", note.get, time, life, time + life))
      case Death(note, time, life) => println(stringPattern.format(time, "-", note.get, time - life, life, time))
      case Decay(time) => println("%5d: %s".format(time, "~~~"))
      case _ => throw sys.error("unknown event type " + e)
    }

    def process: (NoteEvent, DES[NoteEvent]) => DES[NoteEvent] = (e, des) => {
      handler(e)
      des
    }
  }

  val scale = ScaleBuilderRameau(528).build

  val systemProcessor = new HeatedParticleSystemProc(scale)

  implicit val proc = NoteLifePrinter andThen systemProcessor


  implicit val EOrder: Ordering[NoteEvent] =
    Ordering[(Int, Int, Option[NoteEvent.Note])].on[NoteEvent](e => (e.time, e.priority, e.note))

  val notes = "A B C D E F G".split("\\s").toIndexedSeq

  val des = (1 to 10).foldLeft(new DES[NoteEvent](SortedSet())) {
    case (d, i) => {
      val note = notes((Math.random() * notes.size).toInt)
      val bornAt = (Math.random() * 10).toInt
      val lifespan = (Math.random() * 10).toInt
      d addEvent Birth(Option(note), bornAt, lifespan)
    }
  }

  des.play

  val song = systemProcessor.songBuffer.toList.map {
    case (a, b) => (a.toSeq, b)
  }
  println(song)
  DemoChordPlayer.demoPlayChordsSeconds(scale, song)
}

object DesAllScaleNoteEventTester extends App {

  import NoteEvent._


  object NoteLifePrinter extends EventProcessor[NoteEvent] {
    private def stringPattern = "%5d: %s %-2s %5d %5d %5d"

    def handler(e: NoteEvent): Unit = e match {
      case Birth(note, time, life) => println(stringPattern.format(time, "+", note.get, time, life, time + life))
      case Death(note, time, life) => println(stringPattern.format(time, "-", note.get, time - life, life, time))
      case Decay(time) => println("%5d: %s".format(time, "~~~"))
      case _ => throw sys.error("unknown event type " + e)
    }

    def process: (NoteEvent, DES[NoteEvent]) => DES[NoteEvent] = (e, des) => {
      handler(e)
      des
    }
  }

  val scale = ScaleBuilderRameau(528).build

  val systemProcessor = new HeatedAllScaleParticleProc

  implicit val proc = NoteLifePrinter andThen systemProcessor

  implicit val EOrder: Ordering[NoteEvent] =
    Ordering[(Int, Int, Option[NoteEvent.Note])].on[NoteEvent](e => (e.time, e.priority, e.note))

  val notes = "A B C D E F G".split("\\s").toIndexedSeq

  val des = (1 to 10).foldLeft(new DES[NoteEvent](SortedSet())) {
    case (d, i) => {
      val note = notes((Math.random() * notes.size).toInt)
      val bornAt = (Math.random() * 10).toInt
      val lifespan = (Math.random() * 10).toInt
      d addEvent Birth(Option(note), bornAt, lifespan)
    }
  }

  des.play

  val song = systemProcessor.songBuffer.toList.map {
    case (a, b) => (a.toSeq, b)
  }
  println(song)

  val playScale = ScaleBuilderEvenTempFull(528).build
  DemoChordPlayer.demoPlayChordsSeconds(playScale, song)
}
