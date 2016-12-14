package sbn.core.statistics.distributions.exponentialfamily

import breeze.linalg.DenseVector
import sbn.core.variables.Assignments
import sbn.core.variables.model.{ModelVariable, MultinomialType}

/**
  *
  *
  * @param variable
  * @param parents
  * @param assignedDistributions
  */
abstract class EF_BaseDistribution_Multinomial (variable: ModelVariable,
                                                parents: Set[ModelVariable],
                                                assignedDistributions: Map[Assignments, EF_UnivariateDistribution]) extends EF_ConditionalDistribution{

  require(!parents.exists(!_.distributionType.isInstanceOf[MultinomialType]), "Parents must be of multinomial type")

  /** @inheritdoc */
  override val naturalParameters: Vector[DenseVector[Double]] =
    assignedDistributions.values.map(_.naturalParameters).toVector

  /** @inheritdoc */
  override val momentParameters: Vector[DenseVector[Double]] =
    assignedDistributions.values.map(_.momentParameters).toVector

  /** @inheritdoc */
  override def naturalParameters(assignments: Assignments): DenseVector[Double] =
    getEF_UnivariateDistribution(assignments).naturalParameters

  /** @inheritdoc */
  override def momentParameters(assignments: Assignments): DenseVector[Double] =
    getEF_UnivariateDistribution(assignments).momentParameters

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
    getEF_UnivariateDistribution(assignments).logBaseMeasure(x)

  /** @inheritdoc */
  override def logNormalizer(assignments: Assignments): Double =
    getEF_UnivariateDistribution(assignments).logNormalizer

  /** @inheritdoc */
  override def getEF_UnivariateDistribution(assignments: Assignments): EF_UnivariateDistribution = try {
    assignedDistributions(assignments)
  } catch{ case nse: NoSuchElementException => throw new IllegalArgumentException("Invalid assignments for the distribution")}

  }