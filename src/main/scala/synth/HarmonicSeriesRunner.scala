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

    print(
      "-----+-------+---------+-----+-----+-------+-------+---------+-----------\n" +
      " deg-| ratio |  freq-  | oct-| fac-| adju- |  red- | reduced | frequency \n" +
      " ree |       |  uency  | ave | tor | sted  |  uced | decimal | in octave \n" +
      "-----+-------+---------+-----+-----+-------+-------+---------+-----------\n" )

    1 to 32 map { x => series(x) } foreach( note => {

      val str = "%4d %7s %9.2f %5d %5d %7s %7s %9.4f %9.2f".format(
      note.degree
      , asFraction(note.ratioToFundamental)
      , note.simpleFrequency
      , note.octave
      , note.octaveAdjustmentFactor
      , asFraction(note.ratioToFundamentalAdjusted)
      , asFraction(note.ratioToFundamentalAdjustedReduced)
      , note.ratioToFundamentalAdjustedReducedDecimal
      , note.frequency
      )

      println(str)
      })
  }
}
