package ferjorosa.sbn.core.distributions
import java.util.concurrent.ThreadLocalRandom

import ferjorosa.sbn.core.variables.{GaussianType, Variable}

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
  *                                  if the variance is > 0
  */
@throws[IllegalArgumentException]
case class Gaussian(variable: Variable, mean: Double, variance: Double) extends UnivariateDistribution{
  require(variable.distributionType.isInstanceOf[GaussianType])
  require(variance > 0, "Variance > 0")

  /** @inheritdoc */
  override def label: String = "Gaussian"

  /** @inheritdoc */
  override def parameters: Vector[Double] = Vector(this.mean, this.variance)

  /** @inheritdoc */
  override def numberOfParameters: Int = 2

  /**
    * Returns the log probability for a given value
    * @param value the input value.
    * @return the logProbability for a given input value.
    */
  // TODO: check with values out of bounds
  override def getLogProbability(value: Double): Double =
  -0.5 * Math.log(this.variance) - 0.5 * Math.log(2 * Math.PI) - 0.5 * Math.pow(value - this.mean, 2) / this.variance

  /**
    * Returns a randomly sampled value.
    * @return a randomly sampled double value.
    */
  override def sample: Double = (ThreadLocalRandom.current().nextGaussian() * this.standardDeviation) + this.mean

  /**
    * Returns the standard deviation of the distribution.
    * @return the standard deviation of the distribution
    */
  def standardDeviation: Double = Math.sqrt(this.variance)


  def getLogProbability2(value: Double): Double =
    0.5 * Math.sqrt(2 * this.variance * Math.PI) * Math.exp(- Math.pow(value - this.mean, 2) / 2 * this.variance)
}
/** The factory containing specific methods for creating [[Gaussian]] distribution objects */
object Gaussian {

  /**
    * Factory method that produces a new standard [[Gaussian]] distribution, which is a Gaussian distribution with
    * mean = 0 and variance = 1 over tha variable's domain.
    * @param variable the associated variable.
    * @return a new [[Gaussian]] distribution with standard parameter values.
    */
  def apply(variable: Variable): Gaussian = {
    // Standard Gaussian distribution
    Gaussian(variable, 0, 1)
  }
}
