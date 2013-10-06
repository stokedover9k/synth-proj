package gui

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 10/5/13
 * Time: 7:18 PM
 * To change this template use File | Settings | File Templates.
 */

import swing._
import swing.event._
import synth.{PythagoreanSeries, HarmonicSeries, NoteSeries}
import java.awt.BorderLayout
import scala.collection.mutable

object SeriesDemo extends SimpleSwingApplication {

  def fundamental: Float = 528f

  val textArea = new TextArea {
    font = new java.awt.Font("Monospaced", 0, 16)
  }

  val dataTable = new scala.swing.Table

  lazy val columnNames: Seq[String] =
    "degree|ratio|frequency|octave|factor|reduced ratio|reduced decimal|hz in octave".split("\\|")

  def makeRow(note: NoteSeries.Interval) =
    Array[Any](
      note.degree
      , note.generatingExpression
      , note.hzUnscaled
      , note.octave
      , note.octaveAdjustment
      , note.hzFactor
      , note.hzFactor.toFloat
      , note.hz
    )

  val dataFrame = new swing.BoxPanel(Orientation.Vertical) {
//    contents += new ScrollPane(new swing.Table((0 to 32 map (HarmonicSeries(fundamental)(_))).toArray[NoteSeries.Interval] map (makeRow(_)), columnNames))
  }

  def displayData(table: swing.Component): Unit = {
    dataFrame.contents.clear()
    dataFrame.contents += new ScrollPane(table)
    dataFrame.repaint()
    top.pack()
    top.size = dimensions
  }

  val dimensions = new Dimension(900, 400)

  val buttons = new BoxPanel(Orientation.Horizontal) {
    contents += new Button { text = "Harmonic"
      lazy val series = HarmonicSeries(fundamental)
      lazy val rowData =
        (0 to 32 map (series(_))).toArray[NoteSeries.Interval] map (makeRow(_))
      lazy val table = new swing.Table(rowData, columnNames)
      reactions += { case ButtonClicked(_) => displayData(table) }
    }

    contents += new Button { text = "Pythagorean"
      lazy val series = PythagoreanSeries(fundamental)
      lazy val rowData =
        (-1 to 7 map (series(_))).toArray[NoteSeries.Interval] map (makeRow(_))
      lazy val table = new swing.Table(rowData, columnNames)
      reactions += { case ButtonClicked(_) => displayData(table) }
    }
  }

  val top = new MainFrame {

    title = "Series Demo"
    contents = new BorderPanel {

      add(buttons,  BorderPanel.Position.North)
      add(dataFrame, BorderPanel.Position.Center)
    }

    size = dimensions
  }
}
