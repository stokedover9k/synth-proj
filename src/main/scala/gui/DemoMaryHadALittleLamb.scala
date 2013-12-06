package gui

import scala.swing._
import synth.sounds.util.{SoundStream, Formats}
import javax.sound.sampled.AudioFormat
import synth.sounds.{ComplexChord, ComplexTone}
import synth.scales._
import java.util.concurrent.atomic.AtomicReference
import java.awt.Color
import scala.swing.event.ButtonClicked

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 11/22/13
 * Time: 11:05 AM
 * To change this template use File | Settings | File Templates.
 */
object DemoMaryHadALittleLamb extends SimpleSwingApplication {

  val playButton = new Button("PLAY")

  val SAMPLE_RATE = synth.sounds.SAMPLE_RATE

  lazy val toneComponents = synth.sounds.ChordPlayer.toneComponents

  lazy val scales = Map[String, TypedScale](
    "Mean Tone" -> ScaleBuilderMeanTone(528).build
    , "Zarlino" -> ScaleBuilderZarlino(528).build
    , "Rameau" -> ScaleBuilderRameau(528).build
    , "Pythagorean" -> ScaleBuilderPythagHepto(528).build
  )

  val buttons = scales.keys.toSeq.sortBy(_.toString) map (new RadioButton(_))

  val buttonGroup = new ButtonGroup(buttons.toSeq: _*)

  buttons foreach (listenTo(_))

  buttons.head.selected = true

  var scale: TypedScale = scales(buttons.head.text)

  val scaleOptionsPanel = new FlowPanel {
    for (button <- buttons)
      contents += button
  }

  lazy val song: List[(String, Float)] = {
    val notes = "E,D,C,D,E,E,E,D,D,D,E,G,G,E,D,C,D,E,E,E,E,D,D,E,D,C".split(",").toList
    val durations = List(1, 1, 1, 1, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 4) map (_ * .5f)
    notes zip durations
  }

  def chordFrames = song map {
    case (note: String, duration: Float) =>
      ComplexChord(Seq(ComplexTone(scale(note).hz, toneComponents))) -> (duration * SAMPLE_RATE).toInt
  }

  lazy val outFormat = new AudioFormat(SAMPLE_RATE, 8, 1, true, true)

  lazy val line = synth.sounds.getLine(outFormat)

  lazy val player = SoundStream.streamPlayerFor(line)(SAMPLE_RATE.toInt / 8)

  def songStream = SoundStream.getSampleStream(chordFrames) map (Formats.floatConverter(line.getFormat)) flatten

  listenTo(playButton)

  reactions += {
    case ButtonClicked(button) => {
      if (button == playButton) {
        button.enabled = false
        player(new AtomicReference(songStream), -1)

        line.drain()
        button.enabled = true
      }
      else {
        scale = scales(button.text)
      }
    }
    case _ =>
  }

  lazy val notesDisplay = new FlowPanel() {
    contents ++= {
      song map {
        case (note: String, duration: Float) => new Button("") {
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
      }
    }
  }

  def top: Frame = new MainFrame {

    title = "Mary Had a Little Lamb"
    contents = new BorderPanel {
      add(playButton, BorderPanel.Position.South)
      add(scaleOptionsPanel, BorderPanel.Position.Center)
      add(notesDisplay, BorderPanel.Position.North)
    }
  }
}
