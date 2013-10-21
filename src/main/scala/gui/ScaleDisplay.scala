package gui

import scala.Array
import synth.scales.{TypedScale}
import scala.swing.{Font, ScrollPane, Label, BorderPanel}

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 10/14/13
 * Time: 2:54 AM
 * To change this template use File | Settings | File Templates.
 */

class ScaleDisplay(private var scale: TypedScale = null,
                   override var name: String = "")
  extends BorderPanel {

  def setName(str: String): Unit = {
    name = str
    nameLabel.text = "scale: " + str
    nameLabel.repaint()
  }

  def setScale(s: TypedScale): Unit = {
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
    val indices = if (scale == null) 0 until 0 else 0 until scale.size

    indices.toArray map { i =>
      Array[Any](
        "%2s : %-2s".format(scale.getType(i), scale.getName(i))
        , "%.2f".format(scale(i).hz).toFloat
        , scale(i).degree
        , scale(i).hzFactor
        , scale(i).hzFactor.toFloat
      )
    }
  }

  private def getContents: swing.Table = {
    val headings = Seq("note", "hz", "degree", "factor expr", "factor")
    new swing.Table(getDataRows, headings) {
      font = new Font("Monospaced", 0, 14)
    }
  }
}
