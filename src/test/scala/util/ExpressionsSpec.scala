package util2

import org.specs2.mutable._
import org.specs2.specification.Scope

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 9/22/13
 * Time: 4:27 PM
 * To change this template use File | Settings | File Templates.
 */

class ExpressionsSpec extends Specification {

  "Numbers" should {

    "have rigth values" in {
      Num(4).toFloat must_== 4f
      Num(4.5f).toFloat must_== 4.5f
    }
  }

  "Fractions" should {

    trait Fixture extends Scope {

      def checkSigns(n: Int, d: Int): Unit = {
        val f: Fraction = Fraction(n, d)
        if( n * d < 0 ) f.num.toFloat must beLessThan(0f)
        else            f.num.toFloat must beGreaterThanOrEqualTo(0f)
        f.denom.toFloat must beGreaterThan(0f)
      }

    }
    "construct" in {
      val frac: Fraction = new Fraction(Num(1), Num(2))
      frac.num.toFloat must_== 1f
      frac.denom.toFloat must_== 2f
    }

    "reduce at construction" in {
      val frac: Fraction = Fraction(4, 8)
      frac.num.toFloat must_== 1f
      frac.denom.toFloat must_== 2f
    }

    "keep negative sign out of denominator" in new Fixture {
      checkSigns(-4,  8)
      checkSigns(-4, -8)
      checkSigns( 4,  8)
      checkSigns( 4, -8)
    }

  }
}
