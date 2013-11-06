package synth.sounds

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 11/5/13
 * Time: 7:46 PM
 * To change this template use File | Settings | File Templates.
 */
class ClipCollection protected(private val clips: Seq[ClipCollection.Clip] = Seq[ClipCollection.Clip](),
                               val currentOffset: Int = 0) {

  def isEmpty: Boolean = clips.isEmpty

  def addClip(bytes: Array[Byte], offset: Int): ClipCollection = {
    val clip = ClipCollection.Clip(offset, bytes)
    if (clip.end <= currentOffset)
      this
    else
      new ClipCollection(clips :+ clip, currentOffset)
  }

  def addClip(bytes: Array[Byte]): ClipCollection = addClip(bytes, currentOffset)

  def ++(otherCC: ClipCollection): ClipCollection =
    otherCC.clips.foldLeft(this)((cc, clip) => cc.addClip(clip.bytes, clip.begin))

  protected def getSample(offset: Int): Byte = {
    clips.foldLeft(0)((sum, clip) => {
      if (clip.begin <= offset && clip.end > offset)
        sum + clip.bytes(offset - clip.begin)
      else
        sum
    }) match {
      case x if x < Byte.MinValue => Byte.MinValue
      case x if x > Byte.MaxValue => Byte.MaxValue
      case x => x.toByte
    }
  }

  def getSamples(bytes: Array[Byte], n: Int = -1, offset: Int = 0): ClipCollection = {
    val top = if (n < 0) bytes.length else offset + n
    val num = top - offset

    for (i <- 0 until num) {
      bytes(offset + i) = getSample(currentOffset + i)
    }

    ClipCollection(clips, currentOffset + num)
  }
}


object ClipCollection {

  protected case class Clip(begin: Int, bytes: Array[Byte]) {
    def end: Int = begin + bytes.length
  }

  def apply(currentOffset: Int = 0): ClipCollection =
    new ClipCollection(Seq[ClipCollection.Clip](), currentOffset)

  def apply(clips: Seq[ClipCollection.Clip], currentOffset: Int): ClipCollection =
    new ClipCollection(clips.filter(_.end > currentOffset), currentOffset)
}
