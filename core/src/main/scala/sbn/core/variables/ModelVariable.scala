package sbn.core.variables

import java.util.UUID

import sbn.core.data.attributes.Attribute
import sbn.core.statistics.distributions.{ConditionalDistribution, UnivariateDistribution}

/**
  * Created by fer on 2/12/16.
  */
trait ModelVariable extends Variable {

  /**
    * The variable's distribution type (Multinomial, Gaussian, Exponential, etc.).
    *
    * @return the variable's distribution type.
    */
  def distributionType: DistributionType

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
  def newConditionalDistribution(parents: Set[ModelVariable]): ConditionalDistribution = distributionType.newConditionalDistribution(this, parents)
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

  require(distributionType.isAttributeCompatible(attribute), "Attribute is not compatible: "+ distributionType + " & " + attribute.stateSpaceType)
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

  require(distributionType.isAttributeCompatible(attribute), "Attribute is not compatible: "+ distributionType + " & " + attribute.stateSpaceType)
}
