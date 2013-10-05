package synth

/**
* Created with IntelliJ IDEA.
* User: yuriy
* Date: 9/16/13
* Time: 9:30 PM
* To change this template use File | Settings | File Templates.
*/

object SeriesRunner {

  val usage = "[USAGE]: scala synth.SeriesMain <series:[hp]> <fundamental>\n" +
              "         where:\n" +
              "            series         series type identifier (h - Harmonic, p - Pythagorean).\n" +
              "            fundamental    floating point valued fundamental frequency."


  def main(args: Array[String]) = {

    val fundamental = try {
      args(1).toFloat
    } catch {
      case _: Throwable => {
        System.out.println("[Error]: expecting fundamental frequency as argument\n" + usage)
        sys.exit(1)
      }
    }

    val seriesAndRange: (NoteSeries, Range) = try {
      args(0) match {
        case "h" => (HarmonicSeries(fundamental), 0 to 32)
        case "p" => (PythagoreanSeries(fundamental), -1 to 12)
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

    print(
      "-----+------------+----------+-----+-----+----------------+---------+-----------\n" +
      " deg-|    ratio   |   freq-  | oct-| fac-|     reduced    | reduced | frequency \n" +
      " ree |            |   uency  | ave | tor |      ratio     | decimal | in octave \n" +
      "-----+------------+----------+-----+-----+----------------+---------+-----------\n" )

    seriesAndRange._2 map { x => seriesAndRange._1(x) } foreach( note => {

      val str = "%4d %12s %10.2f %5d %5s %16s %9.4f %9.2f".format(
      note.degree
      , note.generatingExpression
      , note.hzUnscaled
      , note.octave
      , note.octaveAdjustment
      , note.hzFactor
      , note.hzFactor.toFloat
      , note.hz
      )

      println(str)
      })
  }
}
