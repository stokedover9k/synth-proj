package gui

import scala.swing._
import scala.swing.event.ButtonClicked

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 10/6/13
 * Time: 2:57 PM
 * To change this template use File | Settings | File Templates.
 */

trait HasData[T] {
  def data: T
}

object RadioDemo extends SimpleSwingApplication {

  def makeButton[T](dataStuff: T, titleStuff: String = null): RadioButton =
    new RadioButton with HasData[T] {
      text = if( titleStuff == null ) dataStuff.toString else titleStuff
      override val data = dataStuff
    }

  val contentPanel = new swing.BoxPanel(Orientation.Vertical)

  trait updatesContent { def updateContent: Unit }

  trait UpdatesContentFrame extends updatesContent {
    def content: Component

    override def updateContent: Unit = {
      contentPanel.contents.clear()
      contentPanel.contents += content
    }
  }

  val button1 = new RadioButton with UpdatesContentFrame {
    text = "Harmonic"
    def content: Component = new Label("Harmonic button")
  }

  val button2 = new RadioButton with UpdatesContentFrame {
    text = "Pythagorean"
    def content: Component = new Label("Pythagorean button")
  }

  val buttons = Seq(button1, button2)

  def top = new MainFrame {

    title = "Radios"

    val contentBox = new BoxPanel(Orientation.Vertical)

    contents = new BoxPanel(Orientation.Vertical) {
      contents ++= buttons
      contents += contentBox
    }


    new ButtonGroup(buttons: _*)

    buttons foreach (listenTo(_))

    reactions += {
      case ButtonClicked(button) => {

        button match {
          case b: UpdatesContentFrame => {
            contentBox.contents.clear()
            contentBox.contents += b.content
            pack()
          }
          case _ =>
        }
      }
    }
  }
}
