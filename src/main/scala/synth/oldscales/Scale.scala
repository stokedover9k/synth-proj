package synth.oldscales

import synth.Series

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 9/30/13
 * Time: 7:25 PM
 * To change this template use File | Settings | File Templates.
 */

abstract class Scale {
  def size: Int
  def apply(index: Int): Series.Interval

  override def toString = (0 until size).map( i => apply(i) ).addString(new StringBuilder, "[", " ", "]").toString
}
