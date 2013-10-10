package synth.scale

import org.specs2.mutable._
import org.specs2.specification.Scope
import synth.scales.{ModeScale, MyScale, ScaleInterface}
import synth.{NoteSeries, PythagoreanSeries}

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 10/9/13
 * Time: 10:23 PM
 * To change this template use File | Settings | File Templates.
 */




object PythagoreanBuilder {

  private val allNotes = "C CD D DE E F FG G GA A AB B C_1".split(" ").toSeq
  private val fullPattern = Seq(1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1)

  protected val heptatonicPattern = Seq(2, 2, 1, 2, 2, 2)  // whole, whole, half, whole, whole, whole
  protected val heptatonicWithOctavePattern = heptatonicPattern ++ Seq(1)

  val fullNoteSet: Seq[Note] = {
    def consumeAndProcess(names: Seq[String], pattern: Seq[Int]): Seq[Note] = pattern match {
      case Seq() => Seq()
      case _ => {
        val newSeq = names.drop(pattern.head)
        val (n, o) = nameAndOctave(newSeq.head)
        Seq(
          if (pattern.head == 0)
            Note.makeFlat(n.substring(1), o).get
          else
            Note.makeNote(n, o).getOrElse(Note.makeSharp(n.substring(0, 1), o).get)
        ) ++ consumeAndProcess(newSeq, pattern.tail)
      }
    }

    def nameAndOctave(name: String) = name.indexOf("_") match {
      case -1 => (name, 0)
      case i => (name.substring(0, i), name.substring(i + 1).toInt)
    }

    {
      val (n, o) = nameAndOctave(allNotes(0))
      Note.makeNote(n, o).get
    } +: consumeAndProcess(allNotes, fullPattern)
  }
}



class NewNotesSpec extends Specification {

  trait Fixture extends Scope {

  }

  "New Notes" should {
    "compile" in new Fixture {
      1 must_== (1)
    }
  }
}
