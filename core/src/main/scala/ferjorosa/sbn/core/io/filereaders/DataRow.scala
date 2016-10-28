package ferjorosa.sbn.core.io.filereaders

import ferjorosa.sbn.core.data.{Attribute, Attributes}


/**
  * this interface defines a row of the data matrix.
  * It represents a specific assignment to all te attributes in the data.
  */
trait DataRow {
  /**
    * Returns the value assigned to a given [[Attribute]] in this DataRow.
    *
    * @param attribute an [[Attribute]] object.
    * @return the assigned value to the given [[Attribute]]. If the attribute is not observed, then returns a Double.NaN value.
    */
  def getValue(attribute: Attribute): Double

  /**
    * Sets the value of an [[Attribute]] in this DataRow.
    * If the value is already included in the data, then the value will be updated accordingly.
    *
    * @param attribute an [[Attribute]] object.
    * @param value a double value to be assigned to the given [[Attribute]].
    */
  def setValue(attribute: Attribute, value: Double)

  /**
    * Returns the set of [[Attributes]] that have observed values in this DataRow.
    *
    * @return a valid [[Attributes]] object.
    */
  def getAttributes: Attributes

  /**
    * Returns all the values of this [[DataRow]] as an array of doubles.
    *
    * @return an array of doubles.
    */
  def toArray: Array[Double]

}
