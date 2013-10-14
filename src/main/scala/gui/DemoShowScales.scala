package gui

import scala.swing._
import synth.scales.{PythagScaleBuilder, Scale}
import scala.swing.event.ButtonClicked

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
    }
    size = new Dimension(600, 400)
  }

  def fundamental = 528f

  val scales = Map[String, () => Scale](
    "Heptatonic (Pythagorean)" -> {
      () => PythagScaleBuilder.heptoScale(fundamental)
    },
    "Full (Pythagorean)" -> {
      () => PythagScaleBuilder.fullScale(fundamental)
    }
  )

  val buttons = scales.keys map (new RadioButton(_))

  new ButtonGroup(buttons.toSeq: _*)

  buttons foreach (listenTo(_))

  reactions += {
    case ButtonClicked(button) => {
      scaleDisplay.setName(button.text)
      scaleDisplay.setScale(scales.get(button.text).get())
    }
    case _ =>
  }

  val scaleChoices = new BoxPanel(Orientation.Horizontal) {
    contents ++= buttons
  }

  val scaleDisplay = new ScaleDisplay()

}
