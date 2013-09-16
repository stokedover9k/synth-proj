package synth

/**
 * Created with IntelliJ IDEA.
 * User: yuriy
 * Date: 9/16/13
 * Time: 9:30 PM
 * To change this template use File | Settings | File Templates.
 */
object HarmonicSeriesRunner {

  val usage = "[USAGE]: scala synth.HarmonicSeriesMain <fundamental>\n" +
              "         where 'fundamental' is a floating point valued fundamental frequency."

  def main(args: Array[String]) = {

    val fundamental = try {
      args(0).toFloat
    } catch {
      case _: Throwable => {
        System.err.println("[Error]: expecting fundamental frequency as argument\n" + usage)
        sys.exit(1)
      }
    }

    val series = HarmonicSeries(fundamental)
    def asFraction(x: (Int, Int)): String = x match { case (x, y) => x + "/" + y }

    1 to 32 map { x => series(x) } foreach( note => {
      val sb: StringBuilder = new StringBuilder

      sb.append(note.degree)
      sb.append(" \t " + asFraction(note.ratioToFundamental))
      sb.append(" \t " + note.simpleFrequency)
      sb.append(" \t " + note.octave)
      sb.append(" \t " + note.octaveAdjustmentFactor)
      sb.append(" \t " + asFraction(note.ratioToFundamentalAdjusted))
      sb.append(" \t " + asFraction(note.ratioToFundamentalAdjustedReduced))
      sb.append(" \t " + note.ratioToFundamentalAdjustedReducedDecimal)
      sb.append(" \t " + note.frequency)

      println(sb.toString)
      })
  }
}
