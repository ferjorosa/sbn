package ferjorosa.sbn.core.variables

import java.util.UUID

import ferjorosa.sbn.core.data.attributes._
import ferjorosa.sbn.core.distributions.{ConditionalDistribution, UnivariateDistribution}

/**
  * This trait defines an interface for both manifest and latent variables.
  */
trait Variable extends Product with Serializable{

  /**
    * The variable's distribution type (Multinomial, Gaussian, Exponential, etc.).
    *
    * @return the variable's distribution type.
    */
  def distributionType: DistributionType

  /**
    * The variable's ID.
    *
    * @return the variable's ID.
    */
  def id: UUID

  /**
    * The [[Attribute]] it was created from.
    *
    * @return the attribute it was created from.
    */
  def attribute: Attribute

  /**
    * The variable's name.
    *
    * @return the variable's name.
    */
  def name: String = this.attribute.name

  /**
    * Returns a [[Boolean]] indicating if the passed value is belongs to the variable's domain (its state space limits).
    *
    * @param value the passed value.
    * @return a [[Boolean]] value indicating if the value belongs to the variable's domain and therefore is permitted.
    */
  def isValuePermitted(value: Double): Boolean = attribute.isValuePermitted(value)

  /**
    * Creates a new [[UnivariateDistribution]] of the distribution type.
    * @return a new [[UnivariateDistribution]] of the distribution type.
    */
  def newUnivariateDistribution: UnivariateDistribution = distributionType.newUnivariateDistribution(this)

  /**
    * Creates a new [[ConditionalDistribution]] whose type is inferred from the variable and its parents.
    *
    * @param parents the parents of the variable.
    * @return a new [[ConditionalDistribution]] whose type is inferred from the variable and its parents.
    */
  def newConditionalDistribution(parents: Set[Variable]): ConditionalDistribution = distributionType.newConditionalDistribution(this, parents)
}

/**
  * This class represents a variable that can be observed and directly measured. Manifest variables should be created from
  * data attributes, because they are their direct representation in the model.
  *
  * @param attribute the [[Attribute]] used to create the variable.
  * @param distributionType the distribution type of the variable.
  * @param id the variable's ID.
  */
case class ManifestVariable (attribute: Attribute,
                             distributionType: DistributionType,
                             id: UUID) extends Variable


/**
  * This class represents a latent variable.
  *
  * @param attribute the [[Attribute]] used to create the variable.
  * @param distributionType the distribution type of the variable.
  * @param id the variable's ID.
  */
case class LatentVariable (attribute: Attribute,
                           distributionType: DistributionType,
                           id: UUID) extends Variable


/**
  * The [[Variable]] factory. It is designed to create manifest and latent variables in a different way. Manifest variables
  * are created from a [[Attribute]], which comes from a DataSource, while Latent variables are created by the user,
  * specifying its parameters.
  */
object VariableFactory {

  /**
    * Creates a manifest multinomial variable from an attribute.
    *
    * @param attribute the [[Attribute]].
    * @throws IllegalArgumentException if the attribute's [[StateSpaceType]] is not finite.
    * @return a new [[ManifestVariable]] of multinomial type.
    */
  @throws[IllegalArgumentException]
  def newMultinomialVariable(attribute: Attribute): ManifestVariable = {
    require(attribute.stateSpaceType.isInstanceOf[FiniteStateSpace], "attribute's state space must be finite")

    ManifestVariable(attribute, new MultinomialType, UUID.randomUUID())
  }

  /**
    * Creates a latent multinomial variable by specifying its number of states.
    *
    * @param name the name of the latent variable.
    * @param nStates the number of states of its associated univariate multinomial distribution.
    * @return a new [[LatentVariable]] of multinomial type.
    */
  def newMultinomialVariable(name: String, nStates: Int): LatentVariable = {
    val attribute = Attribute(name, FiniteStateSpace(nStates))
    LatentVariable(attribute, new MultinomialType, UUID.randomUUID())
  }

  /**
    * Creates a manifest gaussian variable from an attribute.
    *
    * @param attribute the [[Attribute]].
    * @throws IllegalArgumentException if the attribute's [[StateSpaceType]] is not real.
    * @return a new [[ManifestVariable]] of gaussian type.
    */
  @throws[IllegalArgumentException]
  def newGaussianVariable(attribute: Attribute): ManifestVariable = {
    require(attribute.stateSpaceType.isInstanceOf[RealStateSpace], "attribute's state space must be real")
    ManifestVariable(attribute, new GaussianType, UUID.randomUUID())
  }

  /**
    * Creates a latent gaussian variable by specifying its value intervals.
    *
    * @param name the name of the latent variable.
    * @param min the minimum value of the interval.
    * @param max the maximum value of the interval.
    * @return a new [[LatentVariable]] of gaussian type.
    */
  def newGaussianVariable(name: String, min: Double, max: Double): LatentVariable = {
    val attribute = Attribute(name, RealStateSpace(min, max))
    LatentVariable(attribute, new GaussianType, UUID.randomUUID())
  }

  /**
    * Creates a latent gaussian variable with inifinite value intervals.
    *
    * @param name the name of the latent variable.
    * @return a new [[LatentVariable]] of gaussian type.
    */
  def newGaussianVariable(name: String): LatentVariable = {
    val attribute = Attribute(name, RealStateSpace())
    LatentVariable(attribute, new GaussianType, UUID.randomUUID())
  }

}