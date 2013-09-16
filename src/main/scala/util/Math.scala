package util

/**
 * Created with IntelliJ IDEA.
 * User: yuriy
 * Date: 9/16/13
 * Time: 2:51 PM
 * To change this template use File | Settings | File Templates.
 */
object Log2 {
  def apply(x: Int): Int = {
    var bits = x
    var log = 0
    if( ( bits & 0xffff0000 ) != 0 ) { bits >>>= 16; log = 16 }
    if( bits >= 256 ) { bits >>>= 8; log += 8 }
    if( bits >= 16  ) { bits >>>= 4; log += 4 }
    if( bits >= 4   ) { bits >>>= 2; log += 2 }
    log + ( bits >>> 1 )
  }
}

object gdc {
  def apply(a: Int, b: Int): Int = if (b == 0) a else apply(b, a % b)
}