package sbn.core.distributions

import java.util.concurrent.ThreadLocalRandom

import org.apache.commons.math3.distribution.NormalDistribution
import org.apache.commons.math3.util.FastMath
import sbn.core.variables.{GaussianType, Variable}

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
case class Gaussian(variable: Variable, mean: Double, variance: Double) extends UnivariateDistribution{
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

  /**
    * Returns the probability for a given input value.
    *
    * @param value the value. Depending on the univariate distribution, it will be codified differently.
    * @return the probability for a given input value.
    */
  override def probability(value: Double): Double = implementation.probability(value)

  /**
    * Returns the logProbability for a given input value.
    *
    * @param value the value. Depending on the univariate distribution, it will be codified differently.
    * @return the logProbability for a given input value.
    */
  override def logProbability(value: Double): Double = FastMath.log(probability(value))

  /**
    *
    * @param x
    * @param y
    * @return
    */
  override def probability(x: Double, y: Double): Double = implementation.probability(x, y)

  /**
    *
    * @param value
    * @return
    */
  override def cumulativeProbability(value: Double): Double = implementation.cumulativeProbability(value)


  /**
    *
    * @param value
    * @return
    */
  override def density(value: Double): Double = implementation.density(value)

  /**
    *
    * @param value
    * @return
    */
  override def logDensity(value: Double): Double = implementation.logDensity(value)

  /**
    * Returns a randomly sampled value.
    *
    * @return a randomly sampled double value.
    */
  override def sample: Double = (ThreadLocalRandom.current().nextGaussian() * this.standardDeviation) + this.mean


  /**
    * Returns the log probability for a given value
    *
    * @param value the input value.
    * @return the logProbability for a given input value.
    */
  def getLogProbability2(value: Double): Double =
    Math.exp(- Math.pow(value - this.mean, 2) / (2 * this.variance)) / Math.sqrt(2*variance*Math.PI)
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
