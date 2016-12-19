package sbn.core.variables.model

import java.util.UUID

import sbn.core.data.attributes.{Attribute, FiniteStateSpace, RealStateSpace, StateSpaceType}
import sbn.core.statistics.distributions.exponentialfamily.{EF_Dirichlet, EF_UnivariateDistribution}
import sbn.core.statistics.distributions.{ConditionalDistribution, UnivariateDistribution}
import sbn.core.variables.Variable

trait ModelVariable extends Variable {

  /**
    * The variable's distribution type (Multinomial, Gaussian, Exponential, etc.).
    *
    * @return the variable's distribution type.
    */
  def distributionType: DistributionType
  
  /**
    * Creates a new [[UnivariateDistribution]] of the distribution type.
    *
    * @return a new [[UnivariateDistribution]] of the distribution type.
    */
  def newUnivariateDistribution: UnivariateDistribution = distributionType.newUnivariateDistribution(this)

  /**
    * Creates a new [[ConditionalDistribution]] whose type is inferred from the variable and its parents.
    *
    * @param parents the parents of the variable.
    * @return a new [[ConditionalDistribution]] whose type is inferred from the variable and its parents.
    */
  def newConditionalDistribution(parents: Set[ModelVariable]): ConditionalDistribution =
    distributionType.newConditionalDistribution(this, parents)

  /**
    * Creates a new [[EF_UnivariateDistribution]] of the distribution type.
    *
    * @return a new [[EF_UnivariateDistribution]] of the distribution type.
    */
  // TODO: For the moment not all the EF_Dist have an equivalent Dist implementation, but all the Dist have an equivalent EF_Dist implementation.
  def newEFUnivariateDistribution: EF_UnivariateDistribution = distributionType.newEF_UnivariateDisitribution(this)
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
                             id: UUID) extends ModelVariable{

  require(distributionType.isAttributeCompatible(attribute),
    "Attribute is not compatible: "+ distributionType + " & " + attribute.stateSpaceType)
}

/**
  * This class represents a latent variable.
  *
  * @param attribute the [[Attribute]] used to create the variable.
  * @param distributionType the distribution type of the variable.
  * @param id the variable's ID.
  */
case class LatentVariable (attribute: Attribute,
                           distributionType: DistributionType,
                           id: UUID) extends ModelVariable{

  require(distributionType.isAttributeCompatible(attribute),
    "Attribute is not compatible: "+ distributionType + " & " + attribute.stateSpaceType)
}

/**
  * The [[ModelVariable]] factory. It is designed to create manifest and latent variables in a different way. Manifest variables
  * are created from a [[Attribute]], which comes from a DataSource, while Latent variables are created by the user,
  * specifying its parameters.
  */
object ModelVariablesFactory {

  /**
    * Creates a manifest multinomial variable from an attribute.
    *
    * @param attribute the [[Attribute]].
    * @throws IllegalArgumentException if the attribute's [[StateSpaceType]] is not finite.
    * @return a new [[ManifestVariable]] of multinomial type.
    */
  @throws[IllegalArgumentException]
  def newMultinomialMV(attribute: Attribute): ModelVariable =
    ManifestVariable(attribute, new MultinomialType, UUID.randomUUID())

  /**
    * Creates a latent multinomial variable by specifying its number of states.
    *
    * @param name the name of the latent variable.
    * @param nStates the number of states of its associated univariate multinomial distribution.
    * @return a new [[LatentVariable]] of multinomial type.
    */
  def newMultinomialLV(name: String, nStates: Int): ModelVariable = {
    val attribute = Attribute(name, FiniteStateSpace(nStates))
    LatentVariable(attribute, new MultinomialType, UUID.randomUUID())
  }

  /**
    * Creates a manifest gaussian variable from an attribute.
    *
    * @param attribute the [[Attribute]].
    * @return a new [[ManifestVariable]] of gaussian type.
    * @throws IllegalArgumentException if the attribute's [[StateSpaceType]] is not real.
    */
  @throws[IllegalArgumentException]
  def newGaussianMV(attribute: Attribute): ModelVariable =
    ManifestVariable(attribute, new GaussianType, UUID.randomUUID())

  /**
    * Creates a latent gaussian variable by specifying its value intervals.
    *
    * @param name the name of the latent variable.
    * @param min the minimum value of the interval.
    * @param max the maximum value of the interval.
    * @return a new [[LatentVariable]] of gaussian type.
    */
  def newGaussianLV(name: String, min: Double, max: Double): ModelVariable = {
    val attribute = Attribute(name, RealStateSpace(min, max))
    LatentVariable(attribute, new GaussianType, UUID.randomUUID())
  }

  /**
    * Creates a latent gaussian variable with infinite value intervals.
    *
    * @param name the name of the latent variable.
    * @return a new [[LatentVariable]] of gaussian type.
    */
  def newGaussianLV(name: String): ModelVariable = {
    val attribute = Attribute(name, RealStateSpace())
    LatentVariable(attribute, new GaussianType, UUID.randomUUID())
  }

  /**
    * Creates a Manifest gamma variable from an attribute.
    *
    * @param attribute the [[Attribute]].
    * @return a new [[ManifestVariable]] of gamma type.
    * @throws IllegalArgumentException if the attribute's [[StateSpaceType]] is not real.
    */
  @throws[IllegalArgumentException]
  def newGammaMV(attribute: Attribute): ModelVariable =
    ManifestVariable(attribute, new GammaType, UUID.randomUUID())

  /**
    * Creates a latent gamma variable by specifying its value intervals.
    *
    * @param name the name of the latent variable.
    * @param min the minimum value of the interval.
    * @param max the maximum value of the interval.
    * @return a new [[LatentVariable]] of gamma type.
    */
  def newGammaLV(name: String, min: Double, max: Double): ModelVariable = {
    val attribute = Attribute(name, RealStateSpace(min, max))
    LatentVariable(attribute, new GammaType, UUID.randomUUID())
  }

  /**
    * Creates a latent gamma variable with infinite value intervals.
    *
    * @param name the name of the latent variable.
    * @return a new [[LatentVariable]] of gaussian type.
    */
  def newGammaLV(name: String): ModelVariable = {
    val attribute = Attribute(name, RealStateSpace())
    LatentVariable(attribute, new GammaType, UUID.randomUUID())
  }

  /**
    * Create a latent dirichlet variable with a specific number of states.
    *
    * @param name the name of the latent variable.
    * @param nStates the number of states of its associated univariate dirichlet distribution.
    * @return a new [[LatentVariable]] of dirichlet type
    */
  def newDirichletLV(name: String, nStates: Int): ModelVariable = {
    val attribute = Attribute(name, FiniteStateSpace(nStates))
    LatentVariable(attribute, new DirichletType, UUID.randomUUID())
  }

}