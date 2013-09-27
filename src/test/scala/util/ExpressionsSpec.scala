package util2

import org.specs2.mutable._
import org.specs2.specification.Scope
import util.expr._

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
        if( n * d < 0 ) f.leftOp.toFloat must beLessThan(0f)
        else            f.leftOp.toFloat must beGreaterThanOrEqualTo(0f)
        f.rightOp.toFloat must beGreaterThan(0f)
      }

    }
    "construct" in {
      val frac: Fraction = Fraction(Num(1), Num(2))
      frac.leftOp.toFloat must_== 1f
      frac.rightOp.toFloat must_== 2f
    }

    "reduce at construction" in {
      val frac: Fraction = Fraction(4, 8)
      frac.leftOp.toFloat must_== 1f
      frac.rightOp.toFloat must_== 2f
    }

    "keep negative sign out of denominator" in new Fixture {
      checkSigns(-4,  8)
      checkSigns(-4, -8)
      checkSigns( 4,  8)
      checkSigns( 4, -8)
    }
  }

  "Expression conversion to string" should {

    trait Fixture extends Scope {

    }

    "Whole numbers (WholeNum)" in new Fixture {
      val str: String = WholeNum(3).toString
      str must_== "3"
    }

    "Decimal numbers (DecimalNum)" in new Fixture {
      val str: String = DecimalNum(3.5f).toString
      str must_== "3.5"
    }

    "Division (Div)" in new Fixture {
      val str: String = Div(WholeNum(3), WholeNum(5)).toString
      str must_== "3/5"
    }

  }

  "Expression precedence places right parenthesis" should {

    trait Fixture extends Scope {
      implicit def i2e(i: Int): Expr = WholeNum(i)
    }

    "a-b-(c-d)" in new Fixture {
      val str = Dif(Dif(1, 2), Dif(3, 4)).toString
      str must_== "1-2-(3-4)"
    }

    "(a^b)^c^d" in new Fixture {
      val str = Pow(Pow(1, 2), Pow(3, 4)).toString
      str must_== "(1^2)^3^4"
    }

    "(a+b)*(c-d)" in new Fixture {
      val str = Mult(Add(1, 2), Dif(3, 4)).toString must_== "(1+2)*(3-4)"
    }

    "(a+b)^(c*d)" in new Fixture {
      val str = Pow(Add(1, 2), Mult(3, 4)).toString
      str must_== "(1+2)^(3*4)"
    }
  }
}
