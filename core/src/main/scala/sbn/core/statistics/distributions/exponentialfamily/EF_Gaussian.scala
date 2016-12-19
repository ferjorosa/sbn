package sbn.core.statistics.distributions.exponentialfamily

import breeze.linalg.DenseVector
import org.apache.commons.math3.util.FastMath
import sbn.core.statistics.distributions.learning.CE_Distribution
import sbn.core.statistics.distributions.{Distribution, Gaussian}
import sbn.core.variables.model.{GaussianType, ModelVariable}
import sbn.core.variables.{Assignment, Assignments}

/**
  * This class represents a Gaussian distribution in its exponential-family form. The Gaussian (or normal) distribution
  * is a very common continuous probability distribution, mostly because of the central limit theorem. It is composed of
  * two parameters: the mean and the variance. These two parameters represent the 'moment parameters' of the distribution.
  * The natural parameters can be obtained from them.
  *
  * For more information about this distribution or the theorem, visit https://en.wikipedia.org/wiki/Normal_distribution.
  * For more information about the Exponential family, visit https://en.wikipedia.org/wiki/Exponential_family
  *
  * @param variable the associated variable.
  * @param mean the mean value of the distribution, its central value.
  * @param variance the variance of the distribution. It informally measures how far the set of values are spread out from their mean.
  * @throws RuntimeException if the variable is not of [[GaussianType]]
  *                          or if the variance is <= 0
  */
//TODO: put in wikipedia's form (using natural parameters)
case class EF_Gaussian(variable: ModelVariable, mean: Double, variance: Double) extends EF_UnivariateDistribution{
  require(variable.distributionType.isInstanceOf[GaussianType], "Variable must be of GaussianType")
  require(variance > 0, "Variance must be > 0")

  /** @inheritdoc */
  override val naturalParameters: DenseVector[Double] = DenseVector(mean / variance, - 1 / (2 * variance))

  /** @inheritdoc */
  override val momentParameters: DenseVector[Double] = DenseVector[Double](mean, variance)

  /** @inheritdoc */
  override val logNormalizer: Double = FastMath.log(1 / variance) / 2 - (mean * mean / (2 * variance))

  /** @inheritdoc */
  override def sufficientStatistics(x: Double): DenseVector[Double] = DenseVector(x, x * x)

  /** @inheritdoc */
  override def zeroSufficientStatistics: DenseVector[Double] = DenseVector.zeros(2)

  /** @inheritdoc */
  override def generalZeroSufficientStatistics: Map[Assignments, DenseVector[Double]] =
    Map(Assignments(Set.empty[Assignment]) -> this.zeroSufficientStatistics)

  /** @inheritdoc */
  override def logBaseMeasure(x: Double): Double = - FastMath.log(2*FastMath.PI) / 2

  /** @inheritdoc */
  override def update(momentParameters: DenseVector[Double]): EF_UnivariateDistribution =
    EF_Gaussian(this.variable, momentParameters)

  /** @inheritdoc */
  override def toDistribution: Distribution = Gaussian(this.variable, this.mean, this.variance)

  /** @inheritdoc */
  override def toConjugateExponentialDistribution: CE_Distribution = ???
}

/** The factory containing specific methods for creating [[EF_Gaussian]] distribution objects */
object EF_Gaussian {

  /**
    * Factory method that produces a new [[EF_Gaussian]] distribution from a vector of moment parameters.
    *
    * @param variable the distribution's variable.
    * @param momentParameters the moment parameters of the new distribution.
    * @return a new [[EF_Gaussian]] distribution from a vector of moment parameters.
    */
  def apply(variable: ModelVariable, momentParameters: DenseVector[Double]): EF_Gaussian =
    EF_Gaussian(variable, momentParameters(0), momentParameters(1))

  /**
    * Factory method that produces a new standard [[EF_Gaussian]] distribution, which is a Gaussian distribution with
    * mean = 0 and variance = 1 over that variable's domain.
    *
    * @param variable the associated variable.
    * @return a new [[EF_Gaussian]] distribution with standard parameter values.
    */
  def apply(variable: ModelVariable): EF_Gaussian = EF_Gaussian(variable, 0, 1)
}