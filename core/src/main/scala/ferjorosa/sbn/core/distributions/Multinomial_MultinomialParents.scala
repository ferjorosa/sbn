package ferjorosa.sbn.core.distributions

import ferjorosa.sbn.core.data.attributes.FiniteStateSpace
import ferjorosa.sbn.core.utils.Utils
import ferjorosa.sbn.core.variables.{Assignment, Assignments, MultinomialType, Variable}

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
                                          multinomialParents: Set[Variable],
                                          parameterizedConditionalDistributions: Map[Assignments, Multinomial]) extends ConditionalDistribution {

  /** @inheritdoc */
  override def label: String = "Multinomial | Multinomial"

  /** @inheritdoc */
  override def numberOfParameters: Int = this.parameterizedConditionalDistributions.values.map(_.numberOfParameters).sum

  /** @inheritdoc */
  override def parents: Set[Variable] = this.multinomialParents

  /** @inheritdoc */
  @throws[IllegalArgumentException]
  override def getUnivariateDistribution(assignments: Assignments): UnivariateDistribution = getMultinomial(assignments)

  /** @inheritdoc */
  @throws[IllegalArgumentException]
  override def getLogConditionalProbability(assignments: Assignments, value: Double): Double = getMultinomial(assignments).getLogProbability(value)

  /**
    * Returns the requested Multinomial distribution associated to the provided [[Assignments]] object.
    *
    * @param assignments the parent variables and its values.
    * @throws NoSuchElementException if the provided [[Assignments]] object is not valid.
    * @return the requested [[Multinomial]] distribution
    */
  @throws[NoSuchElementException]
  def getMultinomial(assignments: Assignments): Multinomial = try {
    parameterizedConditionalDistributions(assignments)
  } catch{ case nse: NoSuchElementException => throw new IllegalArgumentException("Invalid assignments for the distribution")}
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

    val parametrizedMultinomialDistributions = generateAssignmentCombinations(multinomialParents)
      // .view makes it much faster because it avoids creating intermediate results.
      .view.map(assignments => assignments -> Multinomial(variable)).toMap

    Multinomial_MultinomialParents(variable, multinomialParents, parametrizedMultinomialDistributions)
  }

  /**
    * Auxiliary method that makes use of [[Utils.cartesianProduct]] to generate
    * @param parents the multinomial parents of the variable.
    * @throws IllegalArgumentException if the parents state space is not finite.
    * @return the sequence of possible parent assignments that will be used to create the internal distributions.
    */
  @throws[IllegalArgumentException]
  private def generateAssignmentCombinations(parents: Set[Variable]): Seq[Assignments] = {
    val stateSequences: Set[Vector[Int]] = parents.map(v => v.attribute.stateSpaceType match {
      case finite: FiniteStateSpace => finite.stateIndexes
      case _ => throw new IllegalArgumentException("Parents state space must be finite")
    })
    // First we obtain the cartesian product (all the combinations) of the parents state space values
    Utils.cartesianProduct(stateSequences)
    // and then we zip each state value with its parent variable reference
      .map(stateCombination => parents.zip(stateCombination))
    // After that we create a Seq[Set[Assignment]] objects
      .map(combination => combination. map(variableAndValue => Assignment(variableAndValue._1, variableAndValue._2)))
    // Finally we generate the Seq[Assignments] object that we return
      .map(x => Assignments(x))
  }
}