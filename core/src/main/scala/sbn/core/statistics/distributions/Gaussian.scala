package sbn.core.statistics.distributions

import java.util.concurrent.ThreadLocalRandom

import org.apache.commons.math3.distribution.NormalDistribution
import org.apache.commons.math3.exception.NumberIsTooLargeException
import org.apache.commons.math3.util.FastMath
import sbn.core.variables.{GaussianType, ModelVariable}

/**
  * This class represents a Gaussian distribution. The Gaussian (or normal) distribution is a very common continuous
  * probability distribution mostly because of the central limit theorem. It is composed of two parameters: the mean and the variance.
  *
  * For more information about this distribution or the theorem, visit https://en.wikipedia.org/wiki/Normal_distribution.
  *
  * @param variable the associated variable.
  * @param mean the mean value of the distribution, its central value.
  * @param variance the variance of the distribution. It informally measures how far the set of values are spread out from their mean.
  * @throws IllegalArgumentException if the variable is not of [[GaussianType]] or
  *                                  if the variance is <= 0
  */
@throws[IllegalArgumentException]
case class Gaussian(variable: ModelVariable, mean: Double, variance: Double) extends UnivariateDistribution{
  require(variable.distributionType.isInstanceOf[GaussianType], "Variable must be of GaussianType")
  require(variance > 0, "Variance must be > 0")

  /** The standard deviation of the distribution */
  val standardDeviation: Double = Math.sqrt(this.variance)

  /** Apache implementation of the Gaussian (Normal) distribution */
  private val implementation: NormalDistribution = new NormalDistribution(mean, standardDeviation)

  /** @inheritdoc */
  override def label: String = "Gaussian"

  /** @inheritdoc */
  override def parameters: Vector[Double] = Vector(this.mean, this.variance)

  /** @inheritdoc */
  override def numberOfParameters: Int = 2

  /** @inheritdoc */
  // The returned value will be always 0 given its infinite points (Real distribution). It doesn't make sense to calculate it.
  override def probability(value: Double): Double = implementation.probability(value)

  /** @inheritdoc */
  // The returned value will be always log(0) given its infinite points (Real distribution). It doesn't make sense to calculate it.
  override def logProbability(value: Double): Double = FastMath.log(probability(value))

  /** @inheritdoc */
  override def probability(x: Double, y: Double): Double = try {
    implementation.probability(x, y)
  }catch { case nitle: NumberIsTooLargeException => throw new IllegalArgumentException("Lower endpoint above upper endpoint (x > y)")}

  /** @inheritdoc */
  override def cumulativeProbability(value: Double): Double = implementation.cumulativeProbability(value)

  /** @inheritdoc */
  override def density(value: Double): Double = implementation.density(value)

  /** @inheritdoc */
  override def logDensity(value: Double): Double = implementation.logDensity(value)

  /** @inheritdoc */
  override def sample: Double = (ThreadLocalRandom.current().nextGaussian() * this.standardDeviation) + this.mean
}

/** The factory containing specific methods for creating [[Gaussian]] distribution objects */
object Gaussian {

  /**
    * Factory method that produces a new standard [[Gaussian]] distribution, which is a Gaussian distribution with
    * mean = 0 and variance = 1 over that variable's domain.
    *
    * @param variable the associated variable.
    * @return a new [[Gaussian]] distribution with standard parameter values.
    */
  def apply(variable: ModelVariable): Gaussian = Gaussian(variable, 0, 1)

}
