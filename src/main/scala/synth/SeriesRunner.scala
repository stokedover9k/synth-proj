//package synth
//
///**
// * Created with IntelliJ IDEA.
// * User: yuriy
// * Date: 9/16/13
// * Time: 9:30 PM
// * To change this template use File | Settings | File Templates.
// */
//object HarmonicSeriesRunner {
//
//  val usage = "[USAGE]: scala synth.HarmonicSeriesMain <fundamental>\n" +
//              "         where 'fundamental' is leftOp floating point valued fundamental frequency."
//
//  def main(args: Array[String]) = {
//
//    val fundamental = try {
//      args(0).toFloat
//    } catch {
//      case _: Throwable => {
//        System.err.println("[Error]: expecting fundamental frequency as argument\n" + usage)
//        sys.exit(1)
//      }
//    }
//
//    val series = HarmonicSeries(fundamental)
//    def asFraction(x: (Int, Int)): String = x match { case (x, y) => x + "/" + y }
//
//    print(
//      "-----+-------+---------+-----+-----+-------+-------+---------+-----------\n" +
//      " deg-| ratio |  freq-  | oct-| fac-| adju- |  red- | reduced | frequency \n" +
//      " ree |       |  uency  | ave | tor | sted  |  uced | decimal | in octave \n" +
//      "-----+-------+---------+-----+-----+-------+-------+---------+-----------\n" )
//
//    1 to 32 map { x => series(x) } foreach( note => {
//
//      val str = "%4d %7s %9.2f %5d %5d %7s %7s %9.4f %9.2f".format(
//      note.degree
//      , note.hzSimpleRatio
//      , note.hzSimple
//      , note.octave
//      , note.adjustFactor
//      , note.hzAdjustedRatio
//      , note.hzReducedRatio
//      , note.hzDecimalRatio
//      , note.hz
//      )
//
//      println(str)
//      })
//  }
//}

package synth

import util.expr.Expr

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
        case "h" => (HarmonicSeries(fundamental), 1 to 32)
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
