package synth

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 10/4/13
 * Time: 2:02 PM
 * To change this template use File | Settings | File Templates.
 */

abstract class Tone {
  val hz: Float
}



case class PureTone(override val hz: Float) extends Tone



case class ComplexTone(
                        override val hz: Float,
                        components: Seq[ComplexTone.Component] = Seq(ComplexTone.FundamentalComponent)
                        ) extends Tone {

  def totalAmp: Float = (0f /: components)(_+_.ampRatio)
}

object ComplexTone {

  /*
   * Ratios are in respect to the fundamental component
   */
  case class Component(hzRatio: Float, ampRatio: Float)

  object FundamentalComponent extends Component(1f, 1f)

  def fromPureTone(p: PureTone): ComplexTone =
    ComplexTone(p.hz, Seq(FundamentalComponent))
}