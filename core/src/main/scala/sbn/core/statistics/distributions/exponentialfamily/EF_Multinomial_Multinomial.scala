package sbn.core.statistics.distributions.exponentialfamily

import breeze.linalg.DenseVector
import sbn.core.statistics.distributions.learning.CE_Distribution
import sbn.core.statistics.distributions.{Distribution, Multinomial, Multinomial_Multinomial}
import sbn.core.variables.Assignments
import sbn.core.variables.model.{ModelVariable, MultinomialType}

/**
  * This class extends the [[EF_BaseDistribution_Multinomial]] class and defines the conditional distribution of a
  * variable of [[MultinomialType]] whose parents are of [[MultinomialType]].
  *
  * This distribution is composed of several [[EF_Multinomial]] distributions, each one of them related to an Assignment of
  * its multinomial parents, resulting in a vector of [[EF_Multinomial]] distributions (or a matrix of parameters).
  *
  * @param variable the main variable of the distribution.
  * @param parents its multinomial parents.
  * @param assignedDistributions the resulting gaussian distributions of the variable.
  * @throws Exception if there is a parent whose type is not multinomial or
  *                   if the variable's type is not multinomial.
  */
case class EF_Multinomial_Multinomial(variable: ModelVariable,
                                      parents: Vector[ModelVariable],
                                      assignedDistributions: Map[Assignments, EF_Multinomial]) extends EF_BaseDistribution_Multinomial(variable, parents, assignedDistributions){

  require(variable.distributionType.isInstanceOf[MultinomialType], "Variable must be of multinomial type")

  /** @inheritdoc */
  override def update(momentParameters: Map[Assignments, DenseVector[Double]]): EF_ConditionalDistribution = {
    EF_Multinomial_Multinomial.create(this.variable, this.parents, momentParameters)
  }

  /** @inheritdoc */
  override def toDistribution: Distribution =
    Multinomial_Multinomial(this.variable,
      this.parents,
      this.assignedDistributions.mapValues(_.toDistribution.asInstanceOf[Multinomial]))

  /** @inheritdoc */
  override def toConjugateExponentialDistribution: CE_Distribution = ???
}

/** The factory containing specific methods for creating [[EF_Gaussian_Multinomial]] distribution objects */
object EF_Multinomial_Multinomial {

  /**
    * Factory method that creates a new [[EF_Multinomial_Multinomial]] distribution form its equivalent in non-exponential form.
    *
    * @param distribution the [[Multinomial_Multinomial]] distribution object used to create the new EF distribution.
    * @return a new [[EF_Gaussian_Multinomial]] distribution from a [[Multinomial_Multinomial]] object.
    */
  def apply(distribution: Multinomial_Multinomial): EF_Multinomial_Multinomial = EF_Multinomial_Multinomial(
      distribution.variable,
      distribution.parents,
      distribution.assignedDistributions.map{case (assignment, dist) => (assignment, dist.toEF_Distribution.asInstanceOf[EF_Multinomial])})

  /**
    * Factory method that creates a new [[EF_Multinomial_Multinomial]] distribution from a set of assigned moment parameter
    * vectors.
    *
    * @param variable the distribution's variable.
    * @param parents the conditioning variables.
    * @param momentParameters the moment parameter vectors of the associated [[EF_Multinomial]] distributions
    * @return a new [[EF_Multinomial_Multinomial]] distribution from a set of assigned moment parameter vectors.
    */
  // TODO cambiar porque el tipo de momentParameters no se tiene en cuenta y da duplicado el apply
  def create(variable: ModelVariable, parents: Vector[ModelVariable], momentParameters: Map[Assignments, DenseVector[Double]]): EF_Multinomial_Multinomial =
    EF_Multinomial_Multinomial(variable, parents, momentParameters.mapValues(EF_Multinomial(variable, _)))

}
