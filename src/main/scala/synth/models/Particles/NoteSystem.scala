package synth.models.Particles

import scala.collection.mutable
import synth.models.Particles.DES.EventProcessor
import synth.models.Particles.NoteEvent.{Birth, Death, Decay}
import synth.models.{MarkovModelTester, SuperParticularDissonance2, HeatedChordState, Heat}
import synth.scales.{IntervalType, TypedScale, ScaleBuilderRameau, ScaleBuilderZarlino}
import util.{CounterMap, Counters, Counter}
import scala.collection.mutable.ListBuffer
import util.expr.Expr
import scala.collection.JavaConversions._

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 12/11/13
 * Time: 1:24 PM
 * To change this template use File | Settings | File Templates.
 */

class NoteSystem {

  import NoteEvent.Note

  val playing: mutable.Set[Note] = mutable.Set[Note]()
  val stopping: mutable.Set[Note] = mutable.Set[Note]()
  val deathTimes: mutable.Map[Note, Int] = mutable.Map[Note, Int]()

  override def toString = playing.toString + " " + stopping.toString

  def emit(note: Note, diesAt: Int): Unit = {
    playing += note
    deathTimes += (note -> diesAt)
  }

  def stop(note: Note): Unit = {
    playing -= note
    stopping += note
    deathTimes -= note
  }

  def remove(note: Note): Unit = {
    stopping -= note
  }

  def flushStopped: Unit =
    stopping.clear

}

abstract class NoteSystemProcessor extends EventProcessor[NoteEvent] {
  def noteSystem: NoteSystem

  def process: (NoteEvent, DES[NoteEvent]) => DES[NoteEvent] = (e, des) => e match {
    case e: Birth => processBirth(e, des)
    case e: Death => processDeath(e, des)
    case e: Decay => processDecay(e, des)
    case _ => des
  }

  def processBirth(e: Birth, des: DES[NoteEvent]): DES[NoteEvent]

  def processDeath(e: Death, des: DES[NoteEvent]): DES[NoteEvent]

  def processDecay(e: Decay, des: DES[NoteEvent]): DES[NoteEvent]
}

abstract class HeatedNoteSystemProcessor extends NoteSystemProcessor {

  import NoteEvent.Note

  def noteHeat: Heat[Note]

  def noteSystem: NoteSystem

  val songBuffer = new ListBuffer[(Traversable[Note], Int)]

  override def process: (NoteEvent, DES[NoteEvent]) => DES[NoteEvent] = (e, des) => {
    val newDes = super.process(e, des)

    if (newDes.size > 0 && e.time < newDes.peek.time) {
      val deathTime = if (noteSystem.deathTimes.size > 0) noteSystem.deathTimes.values.min else e.time
      println(noteSystem.deathTimes, e.time, deathTime)
      songBuffer.append(noteSystem.playing.clone() -> (deathTime - e.time))
    }

    newDes
  }

  def processBirth(e: Birth, des: DES[NoteEvent]): DES[NoteEvent] = {
    noteSystem.emit(e.note.get, e.time + e.lifespan)
    noteHeat.heatUp(e.note.get, 1)
    des addEvent Death(e.note, e.time + e.lifespan, e.lifespan)
  }

  def processDeath(e: Death, des: DES[NoteEvent]): DES[NoteEvent] = {
    noteSystem.stop(e.note.get)
    des addEvent Decay(e.time)
  }

  def emit(playing: Traversable[Note], stopping: Traversable[Note], heat: Heat[Note], time: Int): Traversable[Birth]

  def processDecay(e: Decay, des: DES[NoteEvent]): DES[NoteEvent] = {
    noteHeat.cool(noteHeat.totalHeat)
    val newDes =
      emit(noteSystem.playing, noteSystem.stopping, noteHeat, e.time).foldLeft(des) {
        case (d, b) => d addEvent b
      }
    noteSystem.flushStopped
    newDes
  }
}

class HeatedParticleSystemProc(sc: TypedScale) extends HeatedNoteSystemProcessor {

  import NoteEvent._

  var scale = sc

  override val noteHeat: Heat[Note] = new Heat[Note]

  val noteSystem: NoteSystem = new NoteSystem

  val dissonance = HeatedChordState.dissonanceFunction(scale)

  def emit(playing: Traversable[Note], stopping: Traversable[Note], heat: Heat[Note], time: Int): Traversable[Birth] = {
    val state = HeatedChordState(scale, dissonance)(HeatedChordState.allowAllChords)

    //-------- potency ------------------------------

    val potencyCounter = new Counter[Int]
    val min = 0
    val max = state.elements.size - playing.size
    for (i <- min until max)
      potencyCounter.incrementCount(Math.abs(i - stopping.size), Math.pow(.5, i))
    potencyCounter.normalize()
    val potency = Counters.sample(potencyCounter)

    //--------- chord probabilities ------------------------------

    val chordCounter = new Counter[Set[Note]]
    for (ch <- NoteEmitter.possibleChords(state.elements.toSet, potency, playing.toSet)) {
      var dis: Double = 1
      for (i <- ch) {
        for (j <- ch) {
          dis += dissonance.apply(i, j) * (heat(i) + heat(j) + 1)
        }
      }
      dis = dis / (Math.sqrt(ch.size) + 1)
      chordCounter.incrementCount(ch, dis)
    }
    chordCounter.normalize()

    val emitter = new ProbNoteEmitter(playing.toSet, state.elements.toSet, potency, chordCounter)

    val nextChord = emitter.emit

    val emission = nextChord map (note => Birth(Option(note), time, (Math.random() * 5).toInt))

    emission foreach {
      birth => heat.heatUp(birth.note.get, birth.lifespan / 5f)
    }

    heat.cool(heat.totalHeat)

    emission
  }

  //////////// here's something abstract /////////// !!!!!!!!!!!!

  abstract class NoteEmitter[N] {

    // playing notes
    def P: Set[N]

    // all possible notes
    def A: Set[N]

    def PNot: Set[N] = A.toSet -- P

    def potency: Int

    def possibleEmissions: TraversableOnce[Set[N]] = PNot.toSeq.combinations(potency).map(_.toSet)

    def probChord(chord: Set[N]): Double

    def probEmission(emission: Set[N]): Double = probChord(P ++ emission)
  }

  object NoteEmitter {
    def possibleEmissions[N](possibleNotes: Set[N], size: Int): TraversableOnce[Set[N]] =
      possibleNotes.toSeq.combinations(size).map(_.toSet)

    def possibleChords[N](possibleNotes: Set[N], size: Int, playing: Set[N]): TraversableOnce[Set[N]] =
      possibleNotes.toSeq.combinations(size).map(e => e.toSet ++ playing)
  }

  class ProbNoteEmitter[N](override val P: Set[N]
                           , override val A: Set[N]
                           , override val potency: Int
                           , chordDistribution: Counter[Set[N]]
                            )
    extends NoteEmitter[N] {

    def probChord(chord: Set[N]): Double = chordDistribution.getCount(chord)

    def emit: Set[N] = Counters.sample(chordDistribution) -- P
  }

}

class HeatedAllScaleParticleProc extends HeatedParticleSystemProc(ScaleBuilderRameau(528).build) {

  import NoteEvent._

  val allScales = {

    val builder = ScaleBuilderRameau(528)

    val baseScale = builder.build

    var scales = Map[String, TypedScale]()
    def addScale(scale: TypedScale) = {
      scales += (scale.allNames(0) -> scale)
    }

    addScale(baseScale)

    def loopFifths(builder: ScaleBuilderRameau, builtFrom: Expr, num: Int): Unit = if (num > 0) {
      val newBuilder = builder.fifthUpBuilder
      val scale = newBuilder.build
      addScale(scale)
      loopFifths(newBuilder, builtFrom.mult(scale(IntervalType.Fifth).hzFactor), num - 1)
    }

    def loopFourths(builder: ScaleBuilderRameau, builtFrom: Expr, num: Int): Unit = if (num > 0) {
      val newBuilder = builder.fourthUpBuilder
      val scale = newBuilder.build
      addScale(scale)
      loopFifths(newBuilder, builtFrom.mult(scale(IntervalType.Fourth).hzFactor), num - 1)
    }

    loopFifths(builder, baseScale(0).hzFactor, 5)

    loopFourths(builder, baseScale(0).hzFactor, 7)

    println(scales)

    scales
  }

  override val dissonance = HeatedChordState.multiScaleDissonanceFunction(allScales.values)

  override def emit(playing: Traversable[Note], stopping: Traversable[Note], heat: Heat[Note], time: Int): Traversable[Birth] = {

    val scaleDissonance: Counter[String] = new Counter[String]()

    allScales foreach {
      case (root, sc) => {
        for( i <- sc.allNames )
          for( j <- heat.getCounter.keySet() ) {
            scaleDissonance.incrementCount(root, dissonance(i,j) * (heat(i) + heat(j) + 1))
          }
      }
    }

    scaleDissonance.normalize()

    if( Math.random() < .1 ) {
      val newRoot = Counters.sample(scaleDissonance)
      scale = allScales(newRoot)
      println("new root is " + newRoot)
    }

    super.emit(playing, stopping, heat, time)
  }
}


