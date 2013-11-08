package gui

import scala.swing._
import synth.sounds.rhythm.{DrumPattern, DrumPatterns, DrumsDemo}
import synth.sounds.{ClipCollection, AudioBytesArrays}
import java.io.File
import util.expr.Fraction
import java.awt.Color
import scala.swing.event.ButtonClicked

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 11/7/13
 * Time: 11:58 PM
 * To change this template use File | Settings | File Templates.
 */
object DemoShowDrums extends SimpleSwingApplication {
  def top: Frame = new MainFrame {

    title = "Show Drums"
    contents = new BorderPanel {
      add(rhythmsDisplay, BorderPanel.Position.Center)
      add(controlsPane, BorderPanel.Position.South)
    }
  }

  val line = DrumsDemo.getLine(DrumsDemo.outDataFormat)

  def rhythmPatterns =
    DrumsDemo.drumFileNames.map {
      // map to files
      filename => new File(DrumsDemo.kitDir + filename)
    }.map {
      // map to sample arrays
      drumFile => AudioBytesArrays.fromFile(drumFile, DrumsDemo.outDataFormat)
    }.zipWithIndex.map {
      // map to drum patterns
      case (drumSamples, index) => new DrumPattern(DrumPatterns.afrikanPattern, drumSamples) mode index
    }

  def getClipCollection(tempo: Int, SAMPLE_RATE: Float): ClipCollection =
    rhythmPatterns.map {
      // map to clip collection
      _.getClipsAt(tempo, SAMPLE_RATE)
    }.foldLeft(ClipCollection()) {
      // add all clip collections together
      _ ++ _
    }

  lazy val allRhythmFractions = rhythmPatterns map (_.beats)

  case class RhythmEvent(begin: Fraction, end: Fraction) extends Button(begin.toString) {

  }

  def rhythmEvents = {
    rhythmPatterns map {
      pattern => {
        val meter = pattern.meter
        (pattern.beats, pattern.beats.tail :+ meter).zipped.map {
          case (begin, end) => new RhythmEvent(begin, end) {
            background = Color.LIGHT_GRAY
            preferredSize = new Dimension(((end.toFloat - begin.toFloat) / meter.toFloat * 700).toInt, 30)
            font = new Font("Monospaced", 0, 10)
          }
        }
      }
    }
  }

  def rhythmOn(comp: Component): Unit => Unit =
    Unit => {
      comp.background = Color.WHITE
      comp.repaint
    }

  def rhythmOff(comp: Component): Unit => Unit =
    Unit => {
      comp.background = Color.LIGHT_GRAY
      comp.repaint
    }

  def eventQueue(events: Seq[Seq[RhythmEvent]] = rhythmEvents) = {
    events.flatten.map {
      case e: RhythmEvent => Seq(
        e.begin.toFloat -> rhythmOn(e),
        e.end.toFloat -> rhythmOff(e)
      )
    }.flatten.toArray.sortBy(_._1).toList
  }

  def highlightWhilePlayingAt(tempo: Int, meter: Fraction, events: List[(Float, ((Unit) => Unit))], callback: () => Unit): Unit = {
    def time(t: Float) = t * 60 / tempo * meter.denom

    def play(t: Float, events: List[(Float, Unit => Unit)]): Unit = {
      events match {
        case next :: rest => {
          val nextTime = time(next._1)
          if (nextTime > t)
            Thread.sleep(((nextTime - t) * 1000).toLong)
          next._2() // execute event
          play(nextTime, rest)
        }
        case List() =>
      }
    }

    val threadDisplay = new Thread(new Runnable {
      def run() = {
        play(0f, events)
      }
    })

    val threadPlay = new Thread(new Runnable {
      def run() = {

        var cc = getClipCollection(tempo, 44100)
        val audioBytes = new Array[Byte](1024)

        threadDisplay.start

        while (!cc.isEmpty) {
          cc = cc.getSamples(audioBytes)
          line.write(audioBytes, 0, audioBytes.length)
        }

        line.drain()
        threadDisplay.join
        callback()
      }
    })

    threadPlay.start
  }

  val playButton: Button = new Button("PLAY")
  val tempoField: TextField = new TextField("140", 5)

  val controlsPane = new FlowPanel {

    contents += new Label("Tempo: ")
    contents += tempoField
    contents += playButton
  }

  val rhythmsDisplay = new GridPanel(10, 1) {
    contents ++= rhythmEvents map {
      beats => new FlowPanel {
        contents ++= beats
      }
    }

    listenTo(playButton)

    reactions += {
      case ButtonClicked(button) => {

        if (button == playButton) {
          button.enabled = false

          val events = contents.map {
            case panel: FlowPanel => {
              panel.contents.map {
                case b: RhythmEvent => b
                case _ => throw sys.error("oops")
              }
            }
            case _ => throw sys.error("oops")
          }

          val queue = eventQueue(events)

          val tempo: Int = try tempoField.text.toInt catch {
            case _: Throwable => {
              tempoField.text = "140"
              tempoField.repaint
              140
            }
          }

          highlightWhilePlayingAt(tempo, Fraction.improper(12, 8), queue, () => button.enabled = true)
        }
      }
    }
  }
}
