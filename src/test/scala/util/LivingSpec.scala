package util

import org.specs2.mutable.Specification
import au.com.bytecode.opencsv.CSVReader
import java.io.FileReader

/**
 * Created with IntelliJ IDEA.
 * User: stoked
 * Date: 9/21/13
 * Time: 10:08 PM
 * To change this template use File | Settings | File Templates.
 */

abstract class LivingSpec extends Specification {

  def livingSpecFilename: String

  val dataSkipRows: Int
  val dataSkipCols: Int
  val dataRows: Int
  val dataCols: Int

  lazy val livingSpecReader = new CSVReader(new FileReader(livingSpecFilename))

  lazy val livingSpecData: Vector[Array[String]] = {
    var dt = Vector[Array[String]]()
    var row: Array[String] = null
    while( {row = livingSpecReader.readNext(); row} != null ) dt = dt :+ row
    dt
  }

  def seqsMustEqual[T](col1: Seq[T], col2: Seq[T]): Unit = {
    col1.size must_== col2.size
    col1 zip col2 foreach {
      case (a, b) => a must_== b
    }
  }

  def getDataCol(colIndex: Int): Seq[String] = {
    livingSpecData drop dataSkipRows take dataRows map {
      row => row(colIndex + dataSkipCols)
    }
  }

  def getDataRow(rowIndex: Int): Seq[String] = {
    livingSpecData.drop(dataSkipRows)(rowIndex) drop dataSkipCols take dataCols
  }

  def mustEqualCol[T](col: Seq[T], colNum: Int): Unit = {
    seqsMustEqual(col map {
      _.toString
    }, getDataCol(colNum))
  }

  def mustEqualMappedCol[T](f: String => T)(col: Seq[T], colNum: Int): Unit =
    seqsMustEqual(col, getDataCol(colNum) map f)

  def mustEqualMappedRow[T](f: String => T)(row: Seq[T], rowNum: Int): Unit =
    seqsMustEqual(row, getDataRow(rowNum) map f)

  def mustEqualRow[T](row: Seq[T], rowNum: Int): Unit = {
    seqsMustEqual(row map {
      _.toString
    }, getDataRow(rowNum))
  }

  def mustEqualFloatCol(col: Seq[Float], colNum: Int): Unit = {
    val dt = livingSpecData.drop(dataSkipRows) map {
      row => row(colNum).toFloat
    }
    seqsMustEqual(col, dt)
  }

  def quotedSeq[T](seq: Seq[T]): Seq[String] = seq map { s => "\"%s\"".format(s.toString) }

  def floatSeq(seq: Seq[String]): Seq[Float] = seq map { _.toFloat }

}
