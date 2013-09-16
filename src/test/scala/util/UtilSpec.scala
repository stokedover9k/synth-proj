package util

import org.specs2.mutable._
import org.specs2.specification.Scope

/**
 * Created with IntelliJ IDEA.
 * User: yuriy
 * Date: 9/16/13
 * Time: 2:57 PM
 * To change this template use File | Settings | File Templates.
 */

class UtilSpec extends Specification {

  "Log2" should {
    trait Fixture extends Scope

    "compute same logs as floor of Math.log" in new Fixture {
      for( x <- 1 to 64 ) Log2(x) must_== Math.floor(Math.log(x)/Math.log(2)).toInt
    }
  }
}
