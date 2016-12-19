package sbn.core.statistics.distributions
import sbn.core.data.attributes.FiniteStateSpace
import sbn.core.utils.Utils
import sbn.core.variables.model.{ModelVariable, MultinomialType}
import sbn.core.variables.{Assignment, Assignments}

/**
  * This class abstracts the distributions generated from a set of multinomial parents (i.e., [[Multinomial_Multinomial]],
  * [[Gaussian_Multinomial]], etc.). All of them have a similar form and, to reduce the repeated code, this class
  * implements some of their methods.
  *
  * It is composed of several rows, where each of them is an [[UnivariateDistribution]].
  *
  * @param variable the main variable of the distribution.
  * @param parents its multinomial parents.
  * @param assignedDistributions each row represents a [[UnivariateDistribution]], identified by an [[Assignments]] object
  *                              that represents its parent values.
  * @throws RuntimeException if there is a parent whose type is not multinomial
  *                          or if there is a repeated parent
  *                          or if the number of assigned distributions is incorrect.
  */
// TODO: needs a require that tests if the number of distributions is adequate (multinomial index/size)
abstract class BaseDistribution_Multinomial(variable: ModelVariable,
                                            parents: Vector[ModelVariable],
                                            assignedDistributions: Map[Assignments, UnivariateDistribution]) extends ConditionalDistribution {

  require(!parents.exists(!_.distributionType.isInstanceOf[MultinomialType]), "Parents must be of multinomial type")
  require(parents.distinct equals parents, "Parents cannot be repeated")
  require(assignedDistributions.size equals
    parents.map(_.attribute.stateSpaceType.asInstanceOf[FiniteStateSpace].numberOfStates).product,
    "The number of assigned distributions is incorrect")

  /** @inheritdoc */
  override def numberOfParameters: Int = this.assignedDistributions.values.map(_.numberOfParameters).sum

  /** @inheritdoc */
  override def getUnivariateDistribution(assignments: Assignments): UnivariateDistribution = assignedDistributions(assignments)

  /** @inheritdoc */
  override def conditionalProbability(assignments: Assignments, x: Double): Double = Math.exp(logConditionalProbability(assignments, x))

  /** @inheritdoc */
  override def logConditionalProbability(assignments: Assignments, x: Double): Double = getUnivariateDistribution(assignments).logProbability(x)

  /** @inheritdoc */
  override def conditionalProbability(assignments: Assignments, x0: Double, x1: Double): Double = {
    if(x0 > x1) throw new IllegalArgumentException("Lower endpoint above upper endpoint (x0 > x1)")

    cumulativeConditionalProbability(assignments, x1) - cumulativeConditionalProbability(assignments, x0)
  }

  /** @inheritdoc */
  override def cumulativeConditionalProbability(assignments: Assignments, x: Double): Double = getUnivariateDistribution(assignments).cumulativeProbability(x)

  /** @inheritdoc */
  override def conditionalDensity(assignments: Assignments, x: Double): Double = getUnivariateDistribution(assignments).density(x)

  /** @inheritdoc */
  override def logConditionalDensity(assignments: Assignments, x: Double): Double = getUnivariateDistribution(assignments).logDensity(x)
}

object BaseDistribution_Multinomial {

  /**
    * Auxiliary method that makes use of [[Utils.cartesianProduct]] to generate
    *
    * @param parents the multinomial parents of the variable.
    * @throws RuntimeException if the parents state space is not finite.
    * @return the sequence of possible parent assignments that will be used to create the internal distributions.
    */
  def generateAssignmentCombinations(parents: Vector[ModelVariable]): Seq[Assignments] = {
    val stateSequences: Vector[Vector[Int]] = parents.distinct.map(v => v.attribute.stateSpaceType match {
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
      .map(x => Assignments(x.toSet))
  }
}
