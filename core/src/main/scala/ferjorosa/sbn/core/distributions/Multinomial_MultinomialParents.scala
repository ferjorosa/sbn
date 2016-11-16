package ferjorosa.sbn.core.distributions
import ferjorosa.sbn.core.data.attributes.FiniteStateSpace
import ferjorosa.sbn.core.variables.{Assignments, MultinomialType, Variable}

/**
  * This class defines the conditional distribution of a variable of [[MultinomialType]] whose parents are all also of [[MultinomialType]].
  * This distribution is composed of several [[Multinomial]] distributions, each one of them related to an Assignment of
  * the multinomial parents, resulting in a matrix of parameters.
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
  * @param multinomialParents the parents of the variable.
  * @param parameterizedConditionalDistributions the resulting multinomial distributions of the variable.
  */
case class Multinomial_MultinomialParents(variable: Variable,
                                          multinomialParents: Set[Multinomial],
                                          parameterizedConditionalDistributions: Set[ParameterizedConditionalDistribution[Multinomial]]) extends ConditionalDistribution {

  /**
    * Returns the label of the distribution.
    *
    * @return The label of the distribution.
    */
  override def label: String = "Multinomial | Multinomial"

  /**
    * Returns the number of parameters of the distribution.
    *
    * @return The number of parameters of the distribution.
    */
  override def numberOfParameters: Int = ???

  /**
    * Returns a collection
    * @return
    */
  // TODO: Set is invariant in its type
  override def parents: Set[Distribution] = ??? //this.multinomialParents

  /** @inheritdoc */
  override def getUnivariateDistribution(assignments: Assignments): UnivariateDistribution = ???

  /** @inheritdoc */
  override def getLogConditionalProbability(assignments: Assignments, value: Double): Double = ???

  def getMultinomial(assignments: Assignments) = ???
}

/** The factory containing specific methods for creating [[Multinomial_MultinomialParents]] distribution objects */
object Multinomial_MultinomialParents {

  /**
    * Factory method that creates a [[Multinomial_MultinomialParents]] distribution with random parameters.
    *
    * @param variable the main variable of the distribution.
    * @param multinomialParents the conditioning variables
    * @throws IllegalArgumentException if the variable is not [[MultinomialType]] or
    *                                  if parents are not [[MultinomialType]].
    * @return a new [[Multinomial_MultinomialParents]] distribution with random parameters.
    */
  @throws[IllegalArgumentException]
  def apply(variable: Variable, multinomialParents: Set[Variable]): Multinomial_MultinomialParents ={
    require(variable.distributionType.isInstanceOf[MultinomialType], "Variable must be of multinomial type")
    require(!multinomialParents.exists(_.distributionType.isInstanceOf[MultinomialType]), "Parents must be of multinomial type")



    // TODO: Es neceario programar las combinaciones.
    val distributions = multinomialParents.flatMap{
      variable => variable.attribute.stateSpaceType match {
        case finite: FiniteStateSpace => for(i <-0 until finite.numberOfStates) yield {

          Multinomial(variable)
        }
        case _ => throw new IllegalArgumentException("Parents must be of multinomial type")
      }
    }
    val parentsMultinomialDistributions = multinomialParents.map(Multinomial(_))

    //Multinomial_MultinomialParents(variable, parentsMultinomialDistributions, distributions)
    null
  }

  private def generateAssignmentCombinations(variables: Set[Variable]): Assignments = {
    null
  }
}