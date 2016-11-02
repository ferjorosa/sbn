package ferjorosa.sbn.core.data

import java.util.NoSuchElementException

import ferjorosa.sbn.core.data.attributes.{Attributes, FiniteStateSpace, RealStateSpace}

import scala.util.Try

/**
 *
 * @param attributes
 * @param values
 */
case class DataInstance(attributes: Attributes, values: Vector[Double])

/** */
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
   *
   * @param attributes
   * @param line
   * @return
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