package synth

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 10/4/13
 * Time: 11:58 AM
 * To change this template use File | Settings | File Templates.
 */

abstract class Synthesiser {

  val sampleRate: Float

  def notes: Seq[Synthesiser.Note]

  def getSamples(samples: Array[Float]): Synthesiser
}

object Synthesiser {
  case class Note(hz: Float, vol: Float)
}



class WaveSynthesiser private( override val notes: Seq[Synthesiser.Note],
                               override val sampleRate: Float,
                               currentTime: Float,
                               waveBuilder: CreatesWaves,

                               /*
                                * waves and their volumes
                                */
                               private val waves: Seq[(Wave, Float)]
                               ) extends Synthesiser {

  private lazy val totalVolume: Float = (notes map(_.hz)).foldLeft(0f)(_+_)

  override def getSamples(samples: Array[Float]): WaveSynthesiser = {
    def timeOfStep(i: Int): Float = currentTime + i / sampleRate

    0 until samples.size foreach {
      i => {
        def folder(acc: Float, el: (Wave, Float)): Float = {
          el match {
            case (wave: Wave, vol: Float) => wave.at(timeOfStep(i)) * vol
          }
        }
        samples(i) = waves.foldLeft(0f)(folder) / totalVolume
      }
    }

    new WaveSynthesiser(notes, sampleRate, timeOfStep(samples.size), waveBuilder, waves)
  }
}

object WaveSynthesiser {

  def apply( notes: Seq[Synthesiser.Note],
             sampleRate: Float = 44100,
             currentTime: Float = 0,
             waveBuilder: CreatesWaves = Sine
             ) = {
    def notesToWaves = notes map {
      note => (waveBuilder(note.hz), note.vol)
    }
    new WaveSynthesiser(notes, sampleRate, currentTime, waveBuilder, notesToWaves)
  }
}
