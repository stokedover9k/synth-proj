package gui

import scala.swing._
import java.awt.Color
import synth.scales.{ScaleBuilderRameau, TypedScale}
import synth.models.Particles.{DES, HeatedParticleSystemProc, NoteEvent}
import synth.models.Particles.DesNoteEventTester.NoteLifePrinter
import synth.models.Particles.NoteEvent.Birth
import scala.collection.immutable.SortedSet
import javax.swing.BoxLayout
import scala.swing.event.ButtonClicked
import synth.models.DemoChordPlayer

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 12/12/13
 * Time: 2:06 PM
 * To change this template use File | Settings | File Templates.
 */

object DemoParticles extends SimpleSwingApplication {

  class NoteLabel(note: String, duration: Float) extends Button(duration.toString) {
    enabled = false
    preferredSize = new Dimension((duration * 70).toInt, 30)
    background = Color.LIGHT_GRAY
    font = new Font("Monospaced", 0, 10)

    override def paintComponent(g: swing.Graphics2D) = {
      super.paintComponent(g)
      g.setColor(Color.BLACK)
      g.drawString(note, 5, 10)
    }
  }

  class ScaleDisplay(name: String, scale: TypedScale) extends FlowPanel {
    val notes = scale.allNames map (new NoteLabel(_, 1))
    contents ++= notes
  }

  class ChordDisplay(chord: Seq[NoteEvent.Note], duration: Float) extends BoxPanel(Orientation.Vertical) {
    preferredSize = new Dimension((duration * 70).toInt, 100)
    contents += new Label(duration.toString)
    contents ++= (chord map (note => new NoteLabel(note, duration)))
  }

  class SongDisplay(song: List[(Seq[NoteEvent.Note], Int)]) extends FlowPanel {
    contents ++= song map {
      case (chord, duration) => new ChordDisplay(chord, duration * .5f)
    }
  }

  var scale = ScaleBuilderRameau(528).build

  //-----------------------------------

  val systemProcessor = new HeatedParticleSystemProc

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

  import BorderPanel.Position._

  val playButton = new Button("PLAY")

  listenTo(playButton)

  reactions += {
    case ButtonClicked(button) => {
      if (button == playButton) {
        button.enabled = false
        DemoChordPlayer.demoPlayChordsSeconds(scale, song)
        button.enabled = true
      }
    }
    case _ =>
  }

  def top: Frame = new MainFrame {
    title = "Particles Demo"
    contents = new BorderPanel {
      add(playButton, North)
      add(new ScaleDisplay("Rameau", scale), Center)
      add(new SongDisplay(song), South)
    }
  }
}
