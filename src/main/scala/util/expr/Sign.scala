package util.expr

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 9/22/13
 * Time: 1:37 PM
 * To change this template use File | Settings | File Templates.
 */

abstract class Sign { def opposite: Sign }



object PLUS  extends Sign { def opposite = MINUS }

object MINUS extends Sign { def opposite = PLUS  }
