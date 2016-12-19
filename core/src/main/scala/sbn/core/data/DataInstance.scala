package sbn.core.data

import sbn.core.data.attributes.{Attribute, Attributes, FiniteStateSpace, RealStateSpace}

import scala.util.Try

/**
  * This class represents a data row. It consists of a number of Attributes and its corresponding values.
  *
  * @param attributes the [[Attributes]] object representing the columns of the data instance.
  * @param values the values assigned to the columns.
  * @throws RuntimeException if [[attributes]].size != [[values]].size
  */
case class DataInstance (attributes: Attributes, values: Vector[Double]){
  require(attributes.size == values.size, "attributes and values sizes differ")

  /**
    * Returns the value associated to the attribute.
    *
    * @param attribute the column of the data instance.
    * @throws RuntimeException if the attribute doesn't belong to the Attributes object of the DataInstance.
    * @return the value associated to the attribute.
    */
  def value(attribute: Attribute) = values(attributes.indexOf(attribute))
}

/** A factory object containing specific methods for creating [[DataInstance]] objects. */
object DataInstanceFactory {

  /**
   * Factory method that produces a new DataInstance from an ARFF data line.
    *
   * @param attributes the attributes corresponding to the data values.
   * @param line the data file line.
   * @return a Success(DataInstance) object or a Failure(e: Exception) if an exception was thrown in the process.
   */
  def fromARFFDataLine(attributes: Attributes, line: String): Try[DataInstance] = Try {
    var values = Vector.empty[Double]
    val parts: Array[String] = line.split(",").map(_.trim)

    if (parts.length != attributes.size)
      throw new IllegalArgumentException("The number of columns does not match the number of attributes")

    for(i <- parts.indices) {
      if (parts(i) == "?")
        values = values :+ Double.NaN
      else attributes(i).stateSpaceType match {
          case realStateSpace: RealStateSpace => values = values :+ parts(i).toDouble
          case finiteStateSpace: FiniteStateSpace => values = values :+ finiteStateSpace.getIndexOfState(parts(i)).toDouble
      }
    }

    DataInstance(attributes, values)
  }

}