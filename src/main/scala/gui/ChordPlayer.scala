package gui

import scala.swing._
import synth.scales._
import synth.Series
import synth.sounds.{ComplexTone, ComplexChord, Chord}
import synth.Series.Interval
import scala.swing.event.ButtonClicked

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 11/13/13
 * Time: 1:11 PM
 * To change this template use File | Settings | File Templates.
 */

abstract class ChordPlayer {

  def getBaseScale: TypedScale with WesternModes

  def getModeOffsetsForHarmony: Seq[Int]

  def getToneGenerator: Series.Interval => ComplexTone

  def getChords = {
    val baseScale = getBaseScale
    val scales = baseScale +: (getModeOffsetsForHarmony map baseScale.mode)
    val tone = getToneGenerator

    (scales map (_.intervals)).transpose.map {
      intervals => {
        val tones = intervals map tone
        ComplexChord(tones.toIndexedSeq)
      }
    }
  }

  def play: Unit = {
    val line = synth.sounds.getLine
    val buffsPerSec = 10
    val samples = new Array[Float]((synth.sounds.SAMPLE_RATE / buffsPerSec).toInt)

    val chords = getChords

    for (c <- chords) {
      var chord = c
      for (i <- 0 to buffsPerSec) {
        chord = chord.getSamples(samples)
        line.write(samples.map(y => (y * 64).toByte), 0, samples.size)
      }
    }

    line.drain()
    line.stop()
    line.close()
  }

}


class ChordPlayerSolid extends ChordPlayer {

  var baseScale: TypedScale with WesternModes =
    ScaleBuilderWesternHepto(ScaleBuilderPtolemyChromatic(528f).build, IntervalType.getWholeSteps :+ IntervalType.First).build

  def getBaseScale: TypedScale with WesternModes = baseScale

  var modeOffsetsForHarmony: Seq[Int] =
    Seq()

  def getModeOffsetsForHarmony: Seq[Int] = modeOffsetsForHarmony

  var toneGenerator: Interval => ComplexTone =
    i => ComplexTone(i.hz, synth.sounds.ChordPlayer.toneComponents)

  def getToneGenerator: (Interval) => ComplexTone = toneGenerator
}


class ChordPlayerControls extends BorderPanel {

  val chordPlayer = new ChordPlayerSolid

  val playButton = new Button("PLAY")

  val harmonyOffsets = Seq(IntervalType.Fourth, IntervalType.Fifth)

  val harmonyButtons = harmonyOffsets map {
    i => (new ToggleButton(i.name), i)
  } toMap

  val toneButtons = Map[AbstractButton, Interval => ComplexTone](
    new ToggleButton("Pure") {
      selected = true
    } -> {
      i: Interval => ComplexTone(i.hz, Seq(ComplexTone.Component(1f, 1f)))
    },
    new ToggleButton("Complex") -> {
      i: Interval => ComplexTone(i.hz, synth.sounds.ChordPlayer.toneComponents)
    }
  )

  val toneButtonsGroup = new ButtonGroup(toneButtons.keys.toSeq: _*)

  // Add all elements into the panel
  //--------------------------------
  add(playButton, BorderPanel.Position.South)

  add(new BoxPanel(Orientation.Vertical) {
    contents += new FlowPanel() {
      contents += new Label("Harmony Interval:")
      contents ++= harmonyButtons.keys
    }
    contents += new FlowPanel() {
      contents += new Label("Tone:")
      contents ++= toneButtons.keys
    }
  }, BorderPanel.Position.Center)
  //--------------------------------

  listenTo(playButton)

  reactions += {
    case ButtonClicked(button) => {
      if (button == playButton && button.enabled) {
        button.enabled = false
        val harmonies = harmonyButtons filterKeys (_.selected) values
        val offsets = harmonies map (chordPlayer.baseScale.allTypes.indexOf(_))

        chordPlayer.modeOffsetsForHarmony = offsets.toSeq

        chordPlayer.toneGenerator = toneButtons.get(toneButtonsGroup.selected.get).get

        chordPlayer.play
        button.enabled = true
      }
    }
    case _ =>
  }
}

object DemoHarmony extends SimpleSwingApplication {

  def top: Frame = new MainFrame {

    title = "Show Scales"
    contents = new ChordPlayerControls
  }
}