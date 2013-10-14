package gui

import scala.Array
import synth.scales.{Note, Scale}
import scala.swing.{ScrollPane, Label, BorderPanel}

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 10/14/13
 * Time: 2:54 AM
 * To change this template use File | Settings | File Templates.
 */

class ScaleDisplay(private var scale: Scale = null,
                   override var name: String = "")
  extends BorderPanel {

  def setName(str: String): Unit = {
    name = str
    nameLabel.text = "scale: " + str
    nameLabel.repaint()
  }

  def setScale(s: Scale): Unit = {
    scale = s
    scalePane.contents = getContents
    scalePane.repaint
  }

  private val nameLabel = new Label()
  setName(name)

  private val scalePane = new ScrollPane(getContents)

  add(nameLabel, BorderPanel.Position.North)
  add(scalePane, BorderPanel.Position.Center)

  private def getDataRows: Array[Array[Any]] = {
    val notes = if (scale == null) Array[Note]() else scale.notes.toArray
    notes map {
      _ match {
        case Note(note, interval) => Array[Any](
          note,
          "%.2f".format(interval.hz).toFloat,
          interval.degree,
          interval.hzFactor,
          interval.hzFactor.toFloat
        )
      }
    }
  }

  private def getContents: swing.Table = {
    val headings = Seq("note", "hz", "degree", "factor expr", "factor")
    new swing.Table(getDataRows, headings)
  }
}
