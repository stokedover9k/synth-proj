package gui

import scala.swing._
import scala.swing.event.ButtonClicked
import synth.sounds.util.{SoundStream, Formats}
import javax.sound.sampled.AudioFormat
import synth.sounds.{ComplexChord, ComplexTone}
import synth.scales.ScaleBuilderMeanTone
import java.util.concurrent.atomic.AtomicReference
import java.awt.Color

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

  lazy val scale = ScaleBuilderMeanTone(528f).build

  lazy val song: List[(String, Float)] = {
    val notes = "E,D,C,D,E,E,E,D,D,D,E,G,G,E,D,C,D,E,E,E,E,D,D,E,D,C".split(",").toList
    val durations = List(1, 1, 1, 1, 1, 1, 2, 1, 1, 2, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 4) map (_ * .5f)
    notes zip durations
  }

  lazy val chordFrames = song map {
    case (note: String, duration: Float) =>
      ComplexChord(Seq(ComplexTone(scale(note).hz, toneComponents))) -> (duration * SAMPLE_RATE).toInt
  }

  lazy val outFormat = new AudioFormat(SAMPLE_RATE, 8, 1, true, true)

  lazy val line = synth.sounds.getLine(outFormat)

  lazy val player = SoundStream.streamPlayerFor(line)(SAMPLE_RATE.toInt / 8)

  def songStream = SoundStream.getSampleStream(chordFrames) map (Formats.floatConverter(line.getFormat)) flatten

  listenTo(playButton)

  reactions += {
    case ButtonClicked(playButton) => {
      playButton.enabled = false
      player(new AtomicReference(songStream), -1)

      line.drain()
      line.stop()
      playButton.enabled = true
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
      add(playButton, BorderPanel.Position.Center)
      add(notesDisplay, BorderPanel.Position.North)
    }
  }
}
