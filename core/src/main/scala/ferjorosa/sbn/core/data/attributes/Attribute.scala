package ferjorosa.sbn.core.data.attributes

/**
  * This class represents an attribute that has been created from a data source (i.e, an ARFF file, a data-stream, etc.).
  * An attribute represents a column in the data matrix. It contains the name and type of the elements it contains
  * (discrete, continuous, etc.).
  *
  * @param name the name of the attribute.
  * @param stateSpaceType the [[StateSpaceType]] object that represents its type.
  */
case class Attribute (name: String, stateSpaceType: StateSpaceType){

  /**
    * Returns a [[Boolean]] indicating if the passed value is belongs to the attribute's state space.
    *
    * @param value the passed value.
    * @return a [[Boolean]] value indicating if the value belongs to the attribute's state space and therefore if it is permitted.
    */
  def isValuePermitted(value: Double) = stateSpaceType.isValuePermitted(value)
}