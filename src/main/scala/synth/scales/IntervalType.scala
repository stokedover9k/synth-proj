package synth.scales

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 10/18/13
 * Time: 10:49 PM
 * To change this template use File | Settings | File Templates.
 */

sealed case class IntervalType(name: String) {
  override val toString = name
}


object IntervalType {

  object First extends IntervalType("1")

  object Minor2 extends IntervalType("m2")

  object Major2 extends IntervalType("M2")

  object Minor3 extends IntervalType("m3")

  object Major3 extends IntervalType("M3")

  object Fourth extends IntervalType("4")

  object Dim5 extends IntervalType("b5")

  object Fifth extends IntervalType("5")

  object Minor6 extends IntervalType("m6")

  object Major6 extends IntervalType("M6")

  object Minor7 extends IntervalType("m7")

  object Major7 extends IntervalType("M7")

  lazy val getAll = Seq(
    First, Minor2, Major2, Minor3, Major3, Fourth, Dim5, Fifth, Minor6, Major6, Minor7, Major7
  )

  def index(interval: IntervalType): Int = getAll.indexOf(interval)
}
