package synth.scales

import scala.collection.immutable
import synth.Series.Interval


/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 10/18/13
 * Time: 11:38 PM
 * To change this template use File | Settings | File Templates.
 */

/*
 * Note that the definitions are potentially very inefficient and are meant as stubs/place-holders.
 */
abstract class Scale {

  def size: Int =
    intervals.size

  def intervals: Seq[Interval]

  def apply(index: Int): Interval =
    intervals(index)
}


/*
 * Note that the definitions are potentially very inefficient and are meant as stubs/place-holders.
 */
abstract class TypedScale extends Scale {

  def getType(index: Int): IntervalType =
    allTypes(index)

  def allTypes: Seq[IntervalType]

  def apply(intervalType: IntervalType): Interval =
    apply(allTypes.indexOf(intervalType))


  def getName(index: Int): String =
    allNames(index)

  def allNames: Seq[String]

  def apply(name: String): Interval =
    apply(allNames.indexOf(name))
}


trait HasModes extends TypedScale {

  def mode(offset: Int): ScaleMode = ScaleMode(this, offset)

}

trait WesternModes extends HasModes {

  def Ionian: ScaleMode
  def Dorian: ScaleMode
  def Phrygian: ScaleMode
  def Lydian: ScaleMode
  def Mixolydian: ScaleMode
  def Aeolian: ScaleMode
  def Locrian: ScaleMode
  def IonianOctave: ScaleMode
}


class OctaveWrappedScale protected(override val intervals: immutable.IndexedSeq[Interval],
                                   override val allTypes: immutable.IndexedSeq[IntervalType],
                                   override val allNames: immutable.IndexedSeq[String])
  extends TypedScale with HasModes


object OctaveWrappedScale {

  def apply(intervals: Seq[Interval], types: Seq[IntervalType], names: Seq[String]): OctaveWrappedScale = {
    if (intervals.size != types.size)
      throw sys.error("expecting as many types as intervals")
    if (intervals.size != names.size)
      throw sys.error("expecting as many names as intervals:\n%s\n%s".format(intervals, names))
    if (intervals(0).octaveUp.hz != intervals(intervals.size - 1).hz)
      throw sys.error("expecting the last interval to be an octave of the first")
    new OctaveWrappedScale(intervals.toIndexedSeq, types.toIndexedSeq, names.toIndexedSeq)
  }
}


class UniqueScale protected(override val intervals: immutable.IndexedSeq[Interval],
                            override val allTypes: immutable.IndexedSeq[IntervalType],
                            override val allNames: immutable.IndexedSeq[String])
  extends TypedScale with HasModes


object UniqueScale {

  def apply(intervals: Seq[Interval], types: Seq[IntervalType], names: Seq[String]): UniqueScale = {
    if (intervals.size != types.size)
      throw sys.error("expecting as many types as intervals")
    if (intervals.size != names.size)
      throw sys.error("expecting as many names as intervals")
    if (intervals.groupBy(_.hz).size != intervals.size)
      throw sys.error("expecting unique frequencies")
    new UniqueScale(intervals.toIndexedSeq, types.toIndexedSeq, names.toIndexedSeq)
  }
}


abstract class ScaleMode(scale: TypedScale, offset: Int) extends TypedScale {

  /*
   * maps index to internal index
   */
  protected def indexer: Int => Int

  override def size: Int = scale.size

  def intervals: Seq[Interval] = 0 until scale.size map {
    i => if (i + offset < size) scale(indexer(i)) else scale(indexer(i)).octaveUp
  }

  def allTypes: Seq[IntervalType] = 0 until scale.size map {
    i => scale.getType(indexer(i))
  }

  def allNames: Seq[String] = 0 until scale.size map {
    i => scale.getName(indexer(i))
  }
}

object ScaleMode {

  def apply(scale: OctaveWrappedScale, offset: Int): ScaleMode =
    new ScaleMode(scale, offset) {
      protected def indexer: Int => Int = index =>
        if (index + offset < size)
          index + offset
        else
          (index + offset + 1) % size
    }

  def apply(scale: UniqueScale, offset: Int): ScaleMode =
    new ScaleMode(scale, offset) {
      protected def indexer: Int => Int = index =>
        if (index + offset < size)
          index + offset
        else
          (index + offset) % size
    }

  def apply(scale: TypedScale, offset: Int): ScaleMode = scale match {
    case s: OctaveWrappedScale => apply(s, offset)
    case s: UniqueScale => apply(s, offset)
    case _ => throw sys.error("don't know how to make a mode out of this scale")
  }
}
