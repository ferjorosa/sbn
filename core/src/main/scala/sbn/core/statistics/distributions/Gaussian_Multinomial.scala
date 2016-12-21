package sbn.core.statistics.distributions

import sbn.core.statistics.distributions.exponentialfamily.EF_Gaussian_Multinomial
import sbn.core.statistics.distributions.learning.CE_Distribution
import sbn.core.variables.Assignments
import sbn.core.variables.model.{GaussianType, ModelVariable, MultinomialType}

/**
  * This class extends the [[BaseDistribution_Multinomial]] class and defines the conditional distribution of a
  * variable of [[GaussianType]] whose parents are of [[MultinomialType]].
  *
  * This distribution is composed of several [[Gaussian]] distributions, each one of them related to an Assignment of
  * the multinomial parents, resulting in a vector of [[Gaussian]] distributions (or a matrix of parameters).
  * 
  * @param variable the main variable of the distribution.
  * @param parents its multinomial parents.
  * @param assignedDistributions the resulting gaussian distributions of the variable.
  * @throws RuntimeException if there is a parent whose type is not multinomial
  *                          or if the variable's type is not gaussian.
  */
case class Gaussian_Multinomial(variable: ModelVariable,
                                parents: Vector[ModelVariable],
                                assignedDistributions: Map[Assignments, Gaussian]) extends BaseDistribution_Multinomial(variable, parents, assignedDistributions){

  require(variable.distributionType.isInstanceOf[GaussianType], "Variable must be of gaussian type")

  /**
    * Returns the label of the distribution.
    *
    * @return The label of the distribution.
    */
  override def label: String = "Gaussian | Multinomial"

  /**
    * Returns the distribution in its Exponential Family form.
    *
    * @return the distribution in its Exponential Family form.
    */
  override def toEF_Distribution: EF_Gaussian_Multinomial = EF_Gaussian_Multinomial(this)

  // TODO: doc
  override def toCE_Distribution: CE_Distribution = ???
}

/** The factory containing specific methods for creating [[Gaussian_Multinomial]] distribution objects */
object Gaussian_Multinomial {

  /**
    * Factory method that creates a [[Gaussian_Multinomial]] distribution with random parameters.
    *
    * @param variable the main variable of the distribution.
    * @param multinomialParents the conditioning variables.
    * @throws RuntimeException if the variable is not [[GaussianType]]
    *                          or if parents are not [[GaussianType]].
    * @return a new [[Gaussian_Multinomial]] distribution with random parameters.
    */
  def apply(variable: ModelVariable, multinomialParents: Vector[ModelVariable]): Gaussian_Multinomial = {

    val parametrizedMultinomialDistributions = BaseDistribution_Multinomial.generateAssignmentCombinations(multinomialParents)
      .view.map(assignments => assignments -> Gaussian(variable)).toMap

    Gaussian_Multinomial(variable, multinomialParents, parametrizedMultinomialDistributions)
  }
}
