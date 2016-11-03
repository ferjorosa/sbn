package ferjorosa.sbn.core.data.attributes

/**
  * An attribute represents a column in the data matrix. It contains the name and type of the elements it contains
  * (discrete, continuous, etc.).
  */
sealed trait Attribute{
  /**
    * Returns the name of the attribute.
    * @return the name of the attribute.
    */
  def name: String

  /**
    * Returns the [[StateSpaceType]] object that represents its type.
    * @return the [[StateSpaceType]] object that represents its type.
    */
  def stateSpaceType: StateSpaceType
}

/**
  * This class represents an attribute that has been created from a data source (i.e, an ARFF file, a data-stream, etc.).
  * It implements the [[Attribute]] trait.
  * @param name the name of the attribute.
  * @param stateSpaceType the [[StateSpaceType]] object that represents its type.
  */
case class ManifestAttribute(name: String, stateSpaceType: StateSpaceType) extends Attribute

/**
  * This class represents a special case. It represents an attribute that has been created inside a latent variable.
  * This class is useful because in algorithms like the Expectation-Maximization [EM]
  * (https://en.wikipedia.org/wiki/Expectation%E2%80%93maximization_algorithm) the original DataSet is completed with data
  * from the Latent variables (LVs), and to do so, these LVs need a companion attribute.
  * @param name the name of the attribute.
  * @param stateSpaceType the [[StateSpaceType]] object that represents its type.
  */
case class LatentAttribute(name: String, stateSpaceType: StateSpaceType) extends Attribute