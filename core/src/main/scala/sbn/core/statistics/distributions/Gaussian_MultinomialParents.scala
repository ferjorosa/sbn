package sbn.core.statistics.distributions

import sbn.core.statistics.distributions.exponentialfamily.EF_Gaussian_Multinomial
import sbn.core.variables.Assignments
import sbn.core.variables.model.{GaussianType, ModelVariable, MultinomialType}

/**
  * This class extends the [[BaseDistribution_MultinomialParents]] class and defines the conditional distribution of a
  * variable of [[GaussianType]] whose parents are of [[MultinomialType]].
  *
  * This distribution is composed of several [[Gaussian]] distributions, each one of them related to an Assignment of
  * the multinomial parents, resulting in a matrix of parameters.
  * 
  * @param variable the main variable of the distribution.
  * @param multinomialParents its multinomial parents.
  * @param assignedDistributions the resulting gaussian distributions of the variable.
  */
case class Gaussian_MultinomialParents(variable: ModelVariable,
                                       multinomialParents: Set[ModelVariable],
                                       assignedDistributions: Map[Assignments, Gaussian]) extends BaseDistribution_MultinomialParents(variable, multinomialParents, assignedDistributions){

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
}

/** The factory containing specific methods for creating [[Gaussian_MultinomialParents]] distribution objects */
object Gaussian_MultinomialParents {

  /**
    * Factory method that creates a [[Gaussian_MultinomialParents]] distribution with random parameters.
    *
    * @param variable the main variable of the distribution.
    * @param multinomialParents the conditioning variables
    * @throws IllegalArgumentException if the variable is not [[GaussianType]] or
    *                                  if parents are not [[GaussianType]].
    * @return a new [[Gaussian_MultinomialParents]] distribution with random parameters.
    */
  @throws[IllegalArgumentException]
  def apply(variable: ModelVariable, multinomialParents: Set[ModelVariable]): Gaussian_MultinomialParents ={

    val parametrizedMultinomialDistributions = BaseDistribution_MultinomialParents.generateAssignmentCombinations(multinomialParents)
      .view.map(assignments => assignments -> Gaussian(variable)).toMap

    Gaussian_MultinomialParents(variable, multinomialParents, parametrizedMultinomialDistributions)
  }

}
