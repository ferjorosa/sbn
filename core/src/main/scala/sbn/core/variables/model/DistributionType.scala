package sbn.core.variables.model

import sbn.core.data.attributes.{Attribute, FiniteStateSpace, RealStateSpace}
import sbn.core.statistics.distributions.exponentialfamily._
import sbn.core.statistics.distributions.{Multinomial_Multinomial, _}

/**
  * Represents the [[UnivariateDistribution]] type of a variable.
  */
trait DistributionType{

  /**
    * Tests whether a given parent is compatible or not. In other words, it checks if the resulting conditional distribution
    * would be allowed by the library or not.
    *
    * @param distributionType the parent's distribution type.
    * @return true if the parent is compatible, false otherwise.
    */
  def isParentCompatible(distributionType: DistributionType): Boolean

  /**
    * Tests whether a given attribute is compatible with the variable's distribution type. To do so, its state-space will be
    * tested against the distribution type.
    *
    * @param attribute the given attribute.
    * @return true if the attribute's state space is compatible with the distribution type, false otherwise.
    */
  def isAttributeCompatible(attribute: Attribute): Boolean

  /**
    * Creates a new [[UnivariateDistribution]] of the distribution type.
    *
    * @param variable the variable used to create the [[UnivariateDistribution]].
    * @return a new [[UnivariateDistribution]] of the distribution type.
    */
  //TODO: this method can only be called by ModelVariables until all the UnivariateDistribution implementations have been done
  def newUnivariateDistribution(variable: ModelVariable): UnivariateDistribution

  /**
    * Creates a new [[ConditionalDistribution]] whose type is inferred from the variable and its parents.
    *
    * @param variable the variable used as base for the distribution.
    * @param parents the parents of the variable.
    * @throws RuntimeException if the parent set is empty
    *                          or if the parent set is not compatible.
    * @return a new [[ConditionalDistribution]] whose type is inferred from the variable and its parents.
    */
  //TODO: this method can only be called by ModelVariables until all the ConditionalDistribution implementations have been done
  def newConditionalDistribution(variable: ModelVariable, parents: Vector[ModelVariable]): ConditionalDistribution

  /**
    * Creates a new [[EF_UnivariateDistribution]] object of the distribution type.
    *
    * @param variable the variable used as base for the distribution.
    * @return a new univariate distribution in its exponential-family form.
    */
  //TODO: This method can be called by both main and parameter variables.
  def newEF_UnivariateDisitribution(variable: ModelVariable): EF_UnivariateDistribution
}

/**
  * This class represents a multinomial distribution type.
  */
case class MultinomialType() extends DistributionType{

  /** @inheritdoc */
  override def isParentCompatible(distributionType: DistributionType): Boolean = distributionType match {
      // resulting distribution: Multinomial_MultinomialParents
      case _: MultinomialType => true
      case _ => false
  }

  /** @inheritdoc */
  override def isAttributeCompatible(attribute: Attribute): Boolean = attribute.stateSpaceType match {
    case _: FiniteStateSpace => true
    case _ => false
  }

  /** @inheritdoc */
  override def newUnivariateDistribution(variable: ModelVariable): Multinomial = Multinomial(variable)

  /** @inheritdoc */
  override def newConditionalDistribution(variable: ModelVariable, parents: Vector[ModelVariable]): ConditionalDistribution = {
    require(parents.nonEmpty, "The parent set cannot be empty")

    val distributionTypes = parents.map(_.distributionType).distinct

    if(distributionTypes.size == 1) distributionTypes.head match {
        case _: MultinomialType => Multinomial_Multinomial(variable, parents)
        case _ => throw new IllegalArgumentException("The parent set is not compatible")
      }
    else throw new IllegalArgumentException("The parent set is not compatible")
  }

  /** @inheritdoc */
  override def newEF_UnivariateDisitribution(variable: ModelVariable): EF_UnivariateDistribution = EF_Multinomial(variable)
}

/**
  * This class represents a gaussian distribution type.
  */
case class GaussianType() extends DistributionType{

  /** @inheritdoc */
  override def isParentCompatible(distributionType: DistributionType): Boolean = distributionType match {
    // resulting distribution: Gaussian_MultinomialParents
    case _: MultinomialType => true
    case _ => false
  }

  /** @inheritdoc */
  override def isAttributeCompatible(attribute: Attribute): Boolean = attribute.stateSpaceType match {
    case _: RealStateSpace => true
    case _ => false
  }

  /** @inheritdoc */
  override def newUnivariateDistribution(variable: ModelVariable): Gaussian = Gaussian(variable)

  /** @inheritdoc */
  override def newConditionalDistribution(variable: ModelVariable, parents: Vector[ModelVariable]): ConditionalDistribution = {
    require(parents.nonEmpty, "The parent set cannot be empty")

    val distributionTypes = parents.map(_.distributionType).distinct

    if(distributionTypes.size == 1) distributionTypes.head match {
      case _: MultinomialType => Gaussian_Multinomial(variable, parents)
      case _ => throw new IllegalArgumentException("The parent set is not compatible")
    }
    else throw new IllegalArgumentException("The parent set is not compatible")
  }

  /** @inheritdoc */
  override def newEF_UnivariateDisitribution(variable: ModelVariable): EF_UnivariateDistribution = EF_Gaussian(variable)
}

/**
  * This class represents a gamma distribution type.
  */
case class GammaType() extends DistributionType {

  /** @inheritdoc */
  override def isParentCompatible(distributionType: DistributionType): Boolean = distributionType match {
    // resulting distribution: Gamma_MultinomialParents
    case _: MultinomialType => true
    case _ => false
  }

  /** @inheritdoc */
  override def isAttributeCompatible(attribute: Attribute): Boolean = attribute.stateSpaceType match {
    case _: RealStateSpace => true
    case _ => false
  }
  /** @inheritdoc */
  override def newUnivariateDistribution(variable: ModelVariable): UnivariateDistribution = Gamma(variable, 1, 1)

  /** @inheritdoc */
  // TODO
  override def newConditionalDistribution(variable: ModelVariable, parents: Vector[ModelVariable]): ConditionalDistribution = ???

  /** @inheritdoc */
  override def newEF_UnivariateDisitribution(variable: ModelVariable): EF_UnivariateDistribution = EF_Gamma(variable, 1, 1)
}

/**
  * This class represents a dirichlet distribution type.
  */
case class DirichletType() extends DistributionType {

  /** @inheritdoc */
  override def isParentCompatible(distributionType: DistributionType): Boolean = false

  /** @inheritdoc */
  override def isAttributeCompatible(attribute: Attribute): Boolean = attribute.stateSpaceType match {
    case _: FiniteStateSpace => true
    case _ => false
  }

  /** @inheritdoc */
  override def newUnivariateDistribution(variable: ModelVariable): UnivariateDistribution = ???

  /** @inheritdoc */
  override def newConditionalDistribution(variable: ModelVariable, parents: Vector[ModelVariable]): ConditionalDistribution = ???

  /** @inheritdoc */
  override def newEF_UnivariateDisitribution(variable: ModelVariable): EF_UnivariateDistribution = EF_Dirichlet(variable)
}