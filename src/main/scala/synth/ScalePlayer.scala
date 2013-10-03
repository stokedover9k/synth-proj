package synth

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 10/3/13
 * Time: 1:34 PM
 * To change this template use File | Settings | File Templates.
 */

object ScalePlayer {

  lazy val usage =
    "[USAGE]: scala synth.ScalePlayer <series:[hp]> <fundamental> [mode=i]\n" +
    "         where:\n" +
    "            series         series type identifier (h - Harmonic, p - Pythagorean).\n" +
    "            fundamental    floating point valued fundamental frequency.\n" +
    "            mode           one of the following:\n" +
    "                              Ionian Dorian Phrygian Lydian Mixolydian Aeolian Locrian IonianOctave\n" +
    "                           or the corresponding shorthands:\n" +
    "                              i d p ly m a lo io"

  val SAMPLE_RATE = Main.SAMPLE_RATE

  def main(args: Array[String]): Unit = {

    val fundamental = try {
      args(1).toFloat
    } catch {
      case _: Throwable => {
        System.out.println("[Error]: expecting fundamental frequency as argument\n" + usage)
        sys.exit(1)
      }
    }

    val scaleBuilder: Float => Scale7 = try {

      args(0) match {
        case "h" => {
          object ScaleBuilder extends Series2Scale7[HarmonicSeries] with HarmonicSeries.Extracts7Notes
          f: Float => ScaleBuilder buildScale( HarmonicSeries(f) )
        }
        case "p" => {
          object ScaleBuilder extends Series2Scale7[PythagoreanSeries] with PythagoreanSeries.Extracts7Notes
          f: Float => ScaleBuilder buildScale( PythagoreanSeries(f) )
        }
        case  s  => {
          System.out.println("[Error]: unknown series type (%s)".format(s))
          sys.exit(2)
        }
      }
    } catch {
      case _: Throwable => {
        System.out.println("[Error]: expecting series type as argument\n" + usage)
        sys.exit(1)
      }
    }

    val baseScale = scaleBuilder(fundamental)

    val modeScale: Scale7 =
      if (args.size > 2)
        args(2) match {
          case "i"  | "0" | "Ionian"       => baseScale.Ionian
          case "d"  | "1" | "Dorian"       => baseScale.Dorian
          case "p"  | "2" | "Phrygian"     => baseScale.Phrygian
          case "ly" | "3" | "Lydian"       => baseScale.Lydian
          case "m"  | "4" | "Mixolydian"   => baseScale.Mixolydian
          case "a"  | "5" | "Aeolian"      => baseScale.Aeolian
          case "lo" | "6" | "Locrian"      => baseScale.Locrian
          case "io" | "7" | "IonianOctave" => baseScale.IonianOctave
          case s => {
            System.out.println("[Error]: unknown mode (%s)".format(s))
            sys.exit(2)
          }
        }
      else baseScale

    val rebuiltScale: Scale7 = scaleBuilder(modeScale(0))

    val line = Main.getLine
    var oscillator: Oscillator = BufferedWaveOscillator(1024, Sine, SAMPLE_RATE, 0)

    val buffersPerSecond = 10
    val samplesPerBuffer = (SAMPLE_RATE / buffersPerSecond).toInt
    val repeatForHalfSecond = buffersPerSecond / 2

    val buffer = new Array[Float](samplesPerBuffer)

    def playScale(scale: Scale): Unit = {
      0 to scale.size map { modeScale(_) } foreach { frequency: Float =>
        0 until repeatForHalfSecond foreach { _ =>
          oscillator = oscillator.getSamples(frequency, buffer)
          line.write(buffer.map( y => (y*0.7*64).toByte), 0, buffer.size)
        }
      }
    }

    playScale(modeScale)
    playScale(rebuiltScale)

    line.drain()
    line.stop()
    line.close()
  }
}
