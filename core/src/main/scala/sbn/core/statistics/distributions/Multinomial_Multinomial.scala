package sbn.core.statistics.distributions

import sbn.core.statistics.distributions.exponentialfamily.EF_Multinomial_Multinomial
import sbn.core.statistics.distributions.learning.CE_Distribution
import sbn.core.variables.Assignments
import sbn.core.variables.model.{ModelVariable, MultinomialType}

/**
  * This class extends the [[BaseDistribution_Multinomial]] class and defines the conditional distribution of a
  * variable of [[MultinomialType]] whose parents are all also of [[MultinomialType]].
  *
  * This distribution is composed of several [[Multinomial]] distributions, each one of them related to an Assignment of
  * the multinomial parents, resulting in a vector of [[Multinomial]] distributions (or a matrix of parameters).
  *
  * For example, if the variable (V) has 3 states (s0, s1, s2) and 1 parent (P) with 2 states (p0, p1), we would have the resulting
  * matrix (the Conditional Probability Table), where each row can be considered a [[Multinomial]] distribution
  * (in this example parameter values are random):
  *
  * <table>
  *   <tr>
  *     <th></th>
  *     <th>s0</th>
  *     <th>s1</th>
  *     <th>s2</th>
  *   </tr>
  *   <tr>
  *     <td>P = p0</td>
  *     <td>0.27</td>
  *     <td>0.43</td>
  *     <td>0.30</td>
  *   </tr>
  *   <tr>
  *     <td>P = p1</td>
  *     <td>0.05</td>
  *     <td>0.35</td>
  *     <td>0.60</td>
  *   </tr>
  * </table>
  *
  * @param variable the main variable of the distribution.
  * @param parents the parents of the variable.
  * @param assignedDistributions the resulting multinomial distributions of the variable.
  * @throws RuntimeException if there is a parent whose type is not multinomial
  *                          or if the variable's type is not multinomial.
  */
case class Multinomial_Multinomial(variable: ModelVariable,
                                   parents: Vector[ModelVariable],
                                   assignedDistributions: Map[Assignments, Multinomial]) extends BaseDistribution_Multinomial(variable, parents, assignedDistributions) {

  require(variable.distributionType.isInstanceOf[MultinomialType], "Variable must be of multinomial type")

  /** @inheritdoc */
  override def label: String = "Multinomial | Multinomial"

  /** @inheritdoc */
  override def toEF_Distribution: EF_Multinomial_Multinomial = EF_Multinomial_Multinomial(this)

  // TODO: doc
  override def toCE_Distribution: CE_Distribution = ???
}

/** The factory containing specific methods for creating [[Multinomial_Multinomial]] distribution objects */
object Multinomial_Multinomial {

  /**
    * Factory method that creates a [[Multinomial_Multinomial]] distribution with random parameters.
    *
    * @param variable the main variable of the distribution.
    * @param multinomialParents the conditioning variables
    * @throws RuntimeException if the variable is not [[MultinomialType]] or
    *                          or if parents are not [[MultinomialType]].
    * @return a new [[Multinomial_Multinomial]] distribution with random parameters.
    */
  def apply(variable: ModelVariable, multinomialParents: Vector[ModelVariable]): Multinomial_Multinomial ={

    val parametrizedMultinomialDistributions = BaseDistribution_Multinomial.generateAssignmentCombinations(multinomialParents)
      // .view makes it much faster because it avoids creating intermediate results.
      .view.map(assignments => assignments -> Multinomial(variable)).toMap

    Multinomial_Multinomial(variable, multinomialParents, parametrizedMultinomialDistributions)
  }
}