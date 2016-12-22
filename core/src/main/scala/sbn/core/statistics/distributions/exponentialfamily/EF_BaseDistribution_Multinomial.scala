package sbn.core.statistics.distributions.exponentialfamily

import breeze.linalg.DenseVector
import sbn.core.data.attributes.FiniteStateSpace
import sbn.core.variables.Assignments
import sbn.core.variables.model.{ModelVariable, MultinomialType}

/**
  * This class abstracts the exponential-family distributions generated from a set of multinomial parents (i.e.,
  * [[EF_Multinomial_Multinomial]], [[EF_Gaussian_Multinomial]], etc.). All of them have a similar form and, to reduce
  * the repeated code, this class implements some of their methods.
  *
  * It is composed of several rows, where each of them is an [[EF_UnivariateDistribution]].
  *
  * @param variable the main variable of the distribution.
  * @param parents its multinomial parents.
  * @param assignedDistributions each row represents a [[EF_UnivariateDistribution]], identified by an [[Assignments]] object
  *                              that represents its parent values.
  * @throws RuntimeException if there is a parent whose type is not multinomial
  *                          or if there is a repeated parent
  *                          or if the number of assigned distributions is incorrect.
  */
abstract class EF_BaseDistribution_Multinomial (variable: ModelVariable,
                                                parents: Vector[ModelVariable],
                                                assignedDistributions: Map[Assignments, EF_UnivariateDistribution]) extends EF_ConditionalDistribution{

  require(!parents.exists(!_.distributionType.isInstanceOf[MultinomialType]), "Parents must be of multinomial type")
  require(parents.distinct equals parents, "Parents cannot be repeated")
  require(assignedDistributions.size equals
    parents.map(_.attribute.stateSpaceType.asInstanceOf[FiniteStateSpace].numberOfStates).product,
    "The number of assigned distributions is incorrect")

  /** @inheritdoc */
  override val momentParameters: Vector[DenseVector[Double]] =
    assignedDistributions.values.map(_.momentParameters).toVector

  /** @inheritdoc */
  override val naturalParameters: Vector[DenseVector[Double]] =
    assignedDistributions.values.map(_.naturalParameters).toVector

  /** @inheritdoc */
  override def naturalParameters(assignments: Assignments): DenseVector[Double] =
    getEF_UnivariateDistribution(assignments).naturalParameters

  /** @inheritdoc */
  override def momentParameters(assignments: Assignments): DenseVector[Double] =
    getEF_UnivariateDistribution(assignments).momentParameters

  /** @inheritdoc */
  override def logNormalizer(assignments: Assignments): Double =
    getEF_UnivariateDistribution(assignments).logNormalizer

  /** @inheritdoc */
  override def sufficientStatistics(assignments: Assignments, x: Double): DenseVector[Double] =
    getEF_UnivariateDistribution(assignments).sufficientStatistics(x)

  /** @inheritdoc */
  override def zeroSufficientStatistics: Vector[DenseVector[Double]] =
    assignedDistributions.values.map(_.zeroSufficientStatistics).toVector

  /** @inheritdoc */
  override def generalZeroSufficientStatistics: Map[Assignments, DenseVector[Double]] =
    assignedDistributions.mapValues(_.zeroSufficientStatistics)

  /** @inheritdoc */
  override def logBaseMeasure(assignments: Assignments, x: Double): Double =
    getEF_UnivariateDistribution(assignments).baseMeasure(x)

  /** @inheritdoc */
  override def getEF_UnivariateDistribution(assignments: Assignments): EF_UnivariateDistribution =
    assignedDistributions(assignments)
}