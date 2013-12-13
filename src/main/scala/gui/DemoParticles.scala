package gui

import scala.swing._
import java.awt.Color
import synth.scales._
import synth.models.Particles.{DES, HeatedParticleSystemProc, NoteEvent}
import synth.models.Particles.DesNoteEventTester.NoteLifePrinter
import scala.collection.immutable.SortedSet
import synth.models.DemoChordPlayer
import scala.swing.event.ButtonClicked
import synth.models.Particles.NoteEvent.Birth

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 12/12/13
 * Time: 2:06 PM
 * To change this template use File | Settings | File Templates.
 */

class ScalePicker(fundamental: Float) extends BoxPanel(Orientation.Vertical) {
  private val scales = Map[String, () => TypedScale](
    "Full (Pythagorean)" -> {
      () => ScaleBuilderPythagFull(fundamental).build
    },
    "Heptatonic (Pythagorean)" -> {
      () => ScaleBuilderPythagHepto(fundamental).build
    },
    "Full (Even Temperment)" -> {
      () => ScaleBuilderEvenTempFull(fundamental).build
    },
    "Full (Dodecophonic)" -> {
      () => ScaleBuilderDodecophonicFull(fundamental).build
    },
    "Long (Harmonic)" -> {
      () => ScaleBuilderHarmonicLong(fundamental, 32).build
    },
    "Chromatic (Ptolemy)" -> {
      () => ScaleBuilderPtolemyChromatic(fundamental).build
    },
    "Zarlino" -> {
      () => ScaleBuilderZarlino(fundamental).build
    },
    "Mean Tone" -> {
      () => ScaleBuilderMeanTone(fundamental).build
    },
    "Rameau" -> {
      () => ScaleBuilderRameau(fundamental).build
    }
  )

  val buttons = scales.keys.toSeq.sortBy(_.toString) map (new RadioButton(_))

  private val buttonGroup = new ButtonGroup(buttons.toSeq: _*)

  buttons foreach (listenTo(_))

  contents ++= buttons

  var scale = getScale(buttons.head.text)
  buttons.head.selected = true

  private def getScale(name: String): TypedScale = scales.get(name).get()

  reactions += {
    case ButtonClicked(button) => scale = getScale(button.text)
  }
}

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

  //-----------------------------------

  val systemProcessor = new HeatedParticleSystemProc

  implicit val proc = NoteLifePrinter andThen systemProcessor

  implicit val EOrder: Ordering[NoteEvent] =
    Ordering[(Int, Int, Option[NoteEvent.Note])].on[NoteEvent](e => (e.time, e.priority, e.note))

  val des = {
    val notes = ScaleBuilderRameau(528f).build.allNames

    (1 to 10).foldLeft(new DES[NoteEvent](SortedSet())) {
      case (d, i) => {
        val note = notes((Math.random() * notes.size).toInt)
        val bornAt = (Math.random() * 10).toInt
        val lifespan = (Math.random() * 10).toInt
        d addEvent Birth(Option(note), bornAt, lifespan)
      }
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
        DemoChordPlayer.demoPlayChordsSeconds(scalePicker.scale, song)
        button.enabled = true
      }
    }
    case _ =>
  }

  val scalePicker = new ScalePicker(528f)

  val songDisplay = new ScrollPane {
    contents = new SongDisplay(song)
    preferredSize = new Dimension(800, 300)
  }

  def top: Frame = new MainFrame {
    title = "Particles Demo"
    contents = new BorderPanel {
      add(playButton, West)
      add(scalePicker, Center)
      add(songDisplay, South)
    }
  }
}
