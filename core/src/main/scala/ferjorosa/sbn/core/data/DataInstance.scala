package ferjorosa.sbn.core.data

import java.util.NoSuchElementException

import ferjorosa.sbn.core.data.attributes.{Attributes, FiniteStateSpace, RealStateSpace}

import scala.util.Try

/**
 * This class represents a data sample. It consists of a number of Attributes and its corresponding values.
 * @param attributes the [[Attributes]] object representing the columns of the data instance.
 * @param values the values assigned to the columns.
 */
case class DataInstance(attributes: Attributes, values: Vector[Double]){
  require(attributes.size == values.size)
}

/** The companion object that contains factory methods for creating [[DataInstance]] objects. */
object DataInstanceFactory {

  @throws[IllegalArgumentException]
  def fromARFFDataLineException(attributes: Attributes, line: String): DataInstance ={
    var values = Vector.empty[Double]
    val parts: Array[String] = line.split(",")

    if (parts.length != attributes.size)
      throw new IllegalArgumentException("The number of columns does not match the number of attributes.")

    for(i <- parts.indices){
      if (parts(i) == "?")
        values = values :+ Double.NaN
      else try{
          attributes.attributeList(i).stateSpaceType match {
            case realStateSpace: RealStateSpace => values = values :+ parts(i).toDouble
            case finiteStateSpace: FiniteStateSpace => values = values :+ finiteStateSpace.getIndexOfState(parts(i)).toDouble
          }
        } catch {
            case nfe: NumberFormatException => throw new IllegalArgumentException("Error when reading value: " + parts(i))
            case nse: NoSuchElementException => throw new IllegalArgumentException("Error when reading value: " + parts(i))
        }
    }

    new DataInstance(attributes, values)
  }

  /**
   * Factory method that produces a new DataInstance from an ARFF data line.
   * @param attributes the attributes corresponding to the data values.
   * @param line the data file line.
   * @return a Success(DataInstance) object or a Failure(e: Exception) if an exception was thrown in the process.
   */
  def fromARFFDataLine(attributes: Attributes, line: String): Try[DataInstance] = Try {
    var values = Vector.empty[Double]
    val parts: Array[String] = line.split(",")

    if (parts.length != attributes.size)
      throw new IllegalArgumentException("The number of columns does not match the number of attributes.")

    for(i <- parts.indices) {
      if (parts(i) == "?")
        values = values :+ Double.NaN
      else attributes(i).stateSpaceType match {
          case realStateSpace: RealStateSpace => values = values :+ parts(i).toDouble
          case finiteStateSpace: FiniteStateSpace => values = values :+ finiteStateSpace.getIndexOfState(parts(i)).toDouble
      }
    }

    new DataInstance(attributes, values)
  }

}