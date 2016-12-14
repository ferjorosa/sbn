package sbn.core.statistics.distributions.exponentialfamily

import breeze.linalg.DenseVector
import sbn.core.statistics.distributions.learning.CE_Distribution
import sbn.core.statistics.distributions.{Distribution, Gaussian, Gaussian_MultinomialParents}
import sbn.core.variables.Assignments
import sbn.core.variables.model.{GaussianType, ModelVariable, MultinomialType}

/**
  * This class extends the [[EF_BaseDistribution_Multinomial]] class and defines the conditional distribution of a
  * variable of [[GaussianType]] whose parents are of [[MultinomialType]].
  *
  * This distribution is composed of several [[EF_Gaussian]] distributions, each one of them related to an Assignment of
  * its multinomial parents, resulting in a vector of [[EF_Gaussian]] distributions (or a matrix of parameters).
  *
  * @param variable the main variable of the distribution.
  * @param parents its multinomial parents.
  * @param assignedDistributions the resulting gaussian distributions of the variable.
  * @throws IllegalArgumentException if there is a parent whose type is not multinomial or
  *                                  if the variable's type is not gaussian.
  */
@throws[IllegalArgumentException]
case class EF_Gaussian_Multinomial(variable: ModelVariable,
                                   parents: Set[ModelVariable],
                                   assignedDistributions: Map[Assignments, EF_Gaussian]) extends EF_BaseDistribution_Multinomial(variable, parents, assignedDistributions) {

  require(variable.distributionType.isInstanceOf[GaussianType], "Variable must be of gaussian type")

  /** @inheritdoc */
  override def update(momentParameters: Map[Assignments, DenseVector[Double]]): EF_ConditionalDistribution =
    EF_Gaussian_Multinomial.create(this.variable, this.parents, momentParameters)

  /** @inheritdoc */
  override def toDistribution: Distribution =
    Gaussian_MultinomialParents(this.variable,
      this.parents,
      this.assignedDistributions.mapValues(_.toDistribution.asInstanceOf[Gaussian]))

  /** @inheritdoc */
  override def toConjugateExponentialDistribution: CE_Distribution = ???
}

/** The factory containing specific methods for creating [[EF_Gaussian_Multinomial]] distribution objects */
object EF_Gaussian_Multinomial {

  /**
    * Factory method that creates a new [[EF_Gaussian_Multinomial]] distribution from its equivalent in non-exponential form.
    *
    * @param distribution the [[Gaussian_MultinomialParents]] distribution object used to create the new EF distribution.
    * @return a new [[EF_Gaussian_Multinomial]] distribution from a [[Gaussian_MultinomialParents]] object.
    */
  def apply(distribution: Gaussian_MultinomialParents): EF_Gaussian_Multinomial = EF_Gaussian_Multinomial(
    distribution.variable,
    distribution.multinomialParents,
    distribution.assignedDistributions.map{case (assignment, dist) => (assignment, dist.toEF_Distribution.asInstanceOf[EF_Gaussian])})

  /**
    * Factory method that creates a new [[EF_Gaussian_Multinomial]] distribution from a set of assigned moment parameter
    * vectors.
    *
    * @param variable the distribution's variable.
    * @param parents the conditioning variables.
    * @param momentParameters the moment parameter vectors of the associated [[EF_Gaussian]] distributions
    * @return a new [[EF_Gaussian_Multinomial]] distribution from a set of assigned moment parameter vectors.
    */
  // TODO: cambiar porque el tipo de momentParameters no se tiene en cuenta y da duplicado el apply
  def create(variable: ModelVariable, parents: Set[ModelVariable], momentParameters: Map[Assignments, DenseVector[Double]]): EF_Gaussian_Multinomial =
    EF_Gaussian_Multinomial(variable, parents, momentParameters.mapValues(EF_Gaussian(variable, _)))

}