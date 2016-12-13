package sbn.core.statistics.exponentialfamily.distributions

import breeze.linalg.DenseVector
import sbn.core.variables.Assignments
import sbn.core.variables.model.{ModelVariable, MultinomialType}

/**
  * Created by fer on 7/12/16.
  */
abstract class EF_BaseDistribution_Multinomial (variable: ModelVariable,
                                            parents: Set[ModelVariable],
                                            parameterizedConditionalDistributions: Map[Assignments, EF_UnivariateDistribution]) extends EF_ConditionalDistribution{

  require(!parents.exists(!_.distributionType.isInstanceOf[MultinomialType]), "Parents must be of multinomial type")

  override val naturalParameters: Vector[DenseVector[Double]] =
    parameterizedConditionalDistributions.values.map(_.naturalParameters).toVector

  override val momentParameters: Vector[DenseVector[Double]] =
    parameterizedConditionalDistributions.values.map(_.momentParameters).toVector

  override def naturalParameters(assignments: Assignments): DenseVector[Double] =
    getEF_UnivariateDistribution(assignments).naturalParameters

  override def momentParameters(assignments: Assignments): DenseVector[Double] =
    getEF_UnivariateDistribution(assignments).momentParameters

  override def sufficientStatistics(assignments: Assignments, x: Double): DenseVector[Double] =
    getEF_UnivariateDistribution(assignments).sufficientStatistics(x)

  override def zeroSufficientStatistics: Vector[DenseVector[Double]] =
    parameterizedConditionalDistributions.values.map(_.zeroSufficientStatistics).toVector

  override def generalZeroSufficientStatistics: Map[Assignments, DenseVector[Double]] =
    parameterizedConditionalDistributions.mapValues(_.zeroSufficientStatistics)

  override def logBaseMeasure(assignments: Assignments, x: Double): Double =
    getEF_UnivariateDistribution(assignments).logBaseMeasure(x)

  override def logNormalizer(assignments: Assignments): Double =
    getEF_UnivariateDistribution(assignments).logNormalizer

  /** @inheritdoc */
  override def getEF_UnivariateDistribution(assignments: Assignments): EF_UnivariateDistribution = try {
    parameterizedConditionalDistributions(assignments)
  } catch{ case nse: NoSuchElementException => throw new IllegalArgumentException("Invalid assignments for the distribution")}

  }