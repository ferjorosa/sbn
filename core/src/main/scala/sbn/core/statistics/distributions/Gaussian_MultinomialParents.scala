package sbn.core.statistics.distributions

import sbn.core.variables.{Assignments, GaussianType, ModelVariable, MultinomialType}

/**
  * Created by fer on 3/11/16.
  */
case class Gaussian_MultinomialParents(variable: ModelVariable,
                                       multinomialParents: Set[ModelVariable],
                                       parameterizedConditionalDistributions: Map[Assignments, Gaussian]) extends BaseDistribution_MultinomialParents(variable, multinomialParents, parameterizedConditionalDistributions){
  /**
    * Returns the label of the distribution.
    *
    * @return The label of the distribution.
    */
  override def label: String = "Gaussian | Multinomial"
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
    require(variable.distributionType.isInstanceOf[GaussianType], "Variable must be of gaussian type")
    require(!multinomialParents.exists(!_.distributionType.isInstanceOf[MultinomialType]), "Parents must be of multinomial type")

    val parametrizedMultinomialDistributions = BaseDistribution_MultinomialParents.generateAssignmentCombinations(multinomialParents)
      // .view makes it much faster because it avoids creating intermediate results.
      .view.map(assignments => assignments -> Gaussian(variable)).toMap

    Gaussian_MultinomialParents(variable, multinomialParents, parametrizedMultinomialDistributions)
  }

}
