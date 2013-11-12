package gui

import scala.swing._
import synth.scales._
import scala.swing.event.ButtonClicked
import scala.swing.event.EditDone

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 10/14/13
 * Time: 1:24 AM
 * To change this template use File | Settings | File Templates.
 */

object DemoShowScales extends SimpleSwingApplication {
  def top: Frame = new MainFrame {

    title = "Show Scales"
    contents = new BorderPanel {
      add(scaleChoices, BorderPanel.Position.North)
      add(scaleDisplay, BorderPanel.Position.Center)
      add(scalePlayer, BorderPanel.Position.South)
    }
    size = new Dimension(600, 600)
  }

  val fundamentalTextField = new TextField("528", 5)
  listenTo(fundamentalTextField)

  def fundamental = try fundamentalTextField.text.toFloat catch {
    case _: Throwable => 0f
  }

  val scales = Map[String, () => TypedScale](
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
    "Zarlino" -> {
      () => ScaleBuilderZarlino(fundamental).build
    }
  )

  val buttons = scales.keys.toSeq.sortBy(_.toString) map (new RadioButton(_))

  val buttonGroup = new ButtonGroup(buttons.toSeq: _*)

  buttons foreach (listenTo(_))

  reactions += {
    case ButtonClicked(button) => {
      if (fundamental != 0f) {
        scaleDisplay.setName(button.text)
        scaleDisplay.setScale(scales.get(button.text).get())
        scalePlayer.setScale(scales.get(button.text).get())
      }
    }
    case EditDone(source: Component) => source match {
      case `fundamentalTextField` => {
        buttonGroup.selected.map(b => b.doClick())
        fundamentalTextField.requestFocusInWindow()
      }
      case _ =>
    }
    case x =>
  }

  val scaleChoices = new BoxPanel(Orientation.Vertical) {

    contents += new BoxPanel(Orientation.Vertical) {
      contents ++= buttons
    }

    contents += new FlowPanel {
      contents += new Label("Fundamental: ")
      contents += fundamentalTextField
    }
  }

  val scaleDisplay = new ScaleDisplay()

  val scalePlayer = new ScalePlayer()

  buttons.seq.head.doClick()
}
