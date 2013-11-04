package gui

import synth.scales.TypedScale
import scala.swing._
import javax.swing.BorderFactory
import synth.sounds.{ToneSynthesiser, WavedToneSynthesiser, ComplexTone}
import scala.swing.event.ButtonClicked
import javax.sound.sampled.{AudioSystem, AudioFormat, DataLine, Clip}

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 10/23/13
 * Time: 7:11 PM
 * To change this template use File | Settings | File Templates.
 */

class PlayerLabel extends Label {
  text = "Stopped"

  val line = synth.sounds.Main.getLineM

  val components =
    Seq(
      ComplexTone.Component(1f, 1f),
      ComplexTone.Component(2f, 0.7f),
      ComplexTone.Component(3f, 0.4f),
      ComplexTone.Component(5f, 0.3f),
      ComplexTone.Component(6f, 0.15f),
      ComplexTone.Component(7f, 0.1f)
    )

  var synthesiser: ToneSynthesiser = WavedToneSynthesiser()

  val perSec = 5
  val samples = new Array[Float](synthesiser.sampleRate.toInt / perSec)

  def playForASecond(f: Float) = {
    def clamp: (Float => Float) = f => Math.max(-1f, Math.min(1f, f))
    val tone = ComplexTone(f, components)
    0 until perSec foreach {
      i => {
        synthesiser = synthesiser.getSamples(tone, samples)
        line.write(samples.map(y => (clamp(y) * 0.5 * 64).toByte), 0, samples.size)
      }
    }
  }

  def playScale(scale: TypedScale, callback: () => Unit) {

    val playNotes = new Thread(new Runnable {

      val displayNoteNames = new Thread(new Runnable {
        def run() = {
          scale.allNames foreach {
            name => {
              text = name
              repaint
              Thread.sleep(1000L)
            }
          }
          text = "Stopped"
          repaint
        }
      })

      def run() = {
        displayNoteNames.start
        scale.intervals foreach (interval => playForASecond(interval.hz))
        displayNoteNames.join

        callback()
      }
    })

    playNotes.start
  }
}

class ScalePlayer(private var scale: TypedScale = null)
  extends BorderPanel {

  /*
   * The panel that displays the notes of the scale.
   */
  private val scaleDisplay = new FlowPanel()

  /*
   * Play button.
   */
  private val controlButtonPlay = new Button() {
    text = "PLAY"
  }

  /*
   * The panel which contains the player controls.
   */
  private val playerControls = new FlowPanel() {
    contents += controlButtonPlay
  }

  /*
   * The component which displays the state of the player (e.g. the note being played).
   */
  private val player = new PlayerLabel {
    listenTo(controlButtonPlay)

    reactions += {
      case ButtonClicked(button) => {
        if (button == controlButtonPlay) {
          button.enabled = false
          playScale(scale, () => button.enabled = true)
        }
      }
      case _ =>
    }
  }

  def setScale(s: TypedScale): Unit = {
    scale = s

    scaleDisplay.contents.clear()
    scaleDisplay.contents ++= scale.allNames map ScalePlayer.makeNoteLabel

    repaint
  }

  add(scaleDisplay, BorderPanel.Position.North)
  add(player, BorderPanel.Position.Center)
  add(playerControls, BorderPanel.Position.South)
}

object ScalePlayer {

  protected def makeNoteLabel(name: String): Label = new Label(name) {
    border = ScalePlayer.labelBorder
  }

  val labelBorder = BorderFactory.createEmptyBorder(10, 10, 10, 10)
}
