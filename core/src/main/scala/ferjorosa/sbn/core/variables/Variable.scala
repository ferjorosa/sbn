package ferjorosa.sbn.core.variables

import java.util.UUID

import ferjorosa.sbn.core.data.attributes._

/**
  * This trait defines an interface for both manifest and latent variables.
  */
trait Variable{

  /**
    * The variable's name.
    * @return the variable's name.
    */
  def name: String

  /**
    * The variable's distribution type (Multinomial, Gaussian, Exponential, etc.).
    * @return the variable's distribution type.
    */
  def distributionType: DistributionType

  /**
    * The variable's ID.
    * @return the variable's ID.
    */
  def id: UUID

  /**
    *
    * @return
    */
  def attribute: Attribute
}

/**
  * This class represents a variable that can be observed and directly measured. Manifest variables should be created from
  * data attributes, because they are their direct representation in the model.
  * @param _attribute The [[ManifestAttribute]] it was created from.
  * @param _distributionType the assigned [[DistributionType]].
  * @param _id the variable's ID.
  */
class ManifestVariable private (val _attribute: ManifestAttribute,
                                val _distributionType: DistributionType,
                                val _id: UUID) extends Variable{

  /** @inheritdoc */
  override def name: String = this._attribute.name

  /** @inheritdoc */
  override def distributionType: DistributionType = this._distributionType

  /** @inheritdoc */
  override def id = this._id

  /** @inheritdoc */
  override def attribute: Attribute = this._attribute
}


/**
  * This class represents a latent variable.
  */
class LatentVariable private (val _attribute: LatentAttribute,
                              val _distributionType: DistributionType,
                              val _id: UUID) extends Variable{

  /** @inheritdoc */
  override def name: String = this._attribute.name

  /** @inheritdoc */
  override def distributionType: DistributionType = this._distributionType

  /** @inheritdoc */
  override def id = this._id

  /** @inheritdoc */
  override def attribute: Attribute = this._attribute
}

/**
  * The [[Variable]] factory. It is designed to create manifest and latent variables in a different way. Manifest variables
  * are created from a [[ManifestAttribute]], which comes from a DataSource, while Latent variables are created by the user,
  * specifying its parameters.
  */
object VariableFactory {

  /**
    * Creates a manifest multinomial variable from a manifest attribute.
    * @param attribute the [[ManifestAttribute]].
    * @throws IllegalArgumentException if the attribute's [[StateSpaceType]] is not finite.
    * @return a new [[ManifestVariable]] of multinomial type.
    */
  @throws[IllegalArgumentException]
  def newMultinomialVariable(attribute: ManifestAttribute): ManifestVariable = {
    require(attribute.stateSpaceType.isInstanceOf[FiniteStateSpace], "attribute's state space must be finite")

    new ManifestVariable(attribute, new MultinomialType)
  }

  /**
    * Creates a latent multinomial variable by specifying its number of states.
    * @param name the name of the latent variable.
    * @param nStates the number of states of its associated univariate multinomial distribution.
    * @return a new [[LatentVariable]] of multinomial type.
    */
  def newMultinomialVariable(name: String, nStates: Int): LatentVariable = {
    val attribute = LatentAttribute(name, FiniteStateSpace(nStates))
    new LatentVariable(attribute, new MultinomialType)
  }

  /**
    * Creates a manifest gaussian variable from a manifest attribute.
    * @param attribute the [[ManifestAttribute]].
    * @throws IllegalArgumentException if the attribute's [[StateSpaceType]] is not real.
    * @return a new [[ManifestVariable]] of gaussian type.
    */
  @throws[IllegalArgumentException]
  def newGaussianVariable(attribute: ManifestAttribute): ManifestVariable = {
    require(attribute.stateSpaceType.isInstanceOf[RealStateSpace], "attribute's state space must be real")
    new ManifestVariable(attribute, new GaussianType)
  }

  /**
    * Creates a latent gaussian variable by specifying its value intervals.
    * @param name the name of the latent variable.
    * @param min the minimum value of the interval.
    * @param max the maximum value of the interval.
    * @return a new [[LatentVariable]] of gaussian type.
    */
  def newGaussianVariable(name: String, min: Double, max: Double): LatentVariable = {
    val attribute = LatentAttribute(name, RealStateSpace(min, max))
    new LatentVariable(attribute, new GaussianType)
  }

  /**
    * Creates a latent gaussian variable with inifinite value intervals.
    * @param name the name of the latent variable.
    * @return a new [[LatentVariable]] of gaussian type.
    */
  def newGaussianVariable(name: String): LatentVariable = {
    val attribute = LatentAttribute(name, RealStateSpace())
    new LatentVariable(attribute, new GaussianType)
  }

}