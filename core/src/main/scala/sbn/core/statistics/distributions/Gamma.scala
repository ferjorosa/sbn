package sbn.core.statistics.distributions

import org.apache.commons.math3.distribution.{GammaDistribution => ApacheGamma}
import org.apache.commons.math3.util.FastMath
import sbn.core.statistics.distributions.exponentialfamily.{EF_Distribution, EF_Gamma}
import sbn.core.variables.model.{GammaType, ModelVariable}

/**
  * This class represents the Gamma distribution. The Gamma distribution is a very common continuous probability distribution
  * because the Exponential and chi-squared distributions are special cases of it. It is composed of two parameters and possess
  * three different parametrizations:
  *
  * 1) With a shape (alpha) parameter and a scale (theta) parameter.
  * 2) With a shape (alpha) parameter and a rate (beta) parameter.
  * 3) With a shape (alpha) parameter and a mean (mu) parameter.
  *
  * This class uses the first parametrization. The second one is mostly used in Bayesian statistics, where the Gamma
  * distribution is used as a conjugate prior distribution for various rate parameters.
  *
  * @param variable the distribution's variable.
  * @param shape the shape parameter of the distribution (more info in https://en.wikipedia.org/wiki/Shape_parameter).
  * @param scale the scale parameter of the distribution (more info in https://en.wikipedia.org/wiki/Scale_parameter).
  * @throws IllegalArgumentException if the variable is not of [[GammaType]] or
  *                                  if the shape is <= 0 or
  *                                  if the scale is <= 0
  */
@throws[IllegalArgumentException]
//TODO: create different constructors for the different parametrizations.
case class Gamma(variable: ModelVariable, shape: Double, scale: Double) extends UnivariateDistribution {

  require(variable.distributionType.isInstanceOf[GammaType], "Variable must be of GammaType")
  require(shape > 0, "Shape must be > 0")
  require(scale > 0, "Scale must be > 0")

  /** This is the inverse of the scale parameter, mostly used in Bayesian statistics. */
  val rate = 1 / scale

  /** This is the mean parameter, which belongs to the third parametrization. */
  val mean = shape / rate

  /** Apache implementation of the Gaussian (Normal) distribution */
  private val implementation: ApacheGamma = new ApacheGamma(shape, scale)

  /** @inheritdoc */
  override def label: String = "Gamma"

  /** @inheritdoc */
  override def parameters: Vector[Double] = Vector(this.shape, this.scale)

  /** @inheritdoc */
  override def numberOfParameters: Int = 2

  /** @inheritdoc */
  override def probability(x: Double): Double = implementation.probability(x)

  /** @inheritdoc */
  override def logProbability(x: Double): Double = FastMath.log(implementation.probability(x))

  /** @inheritdoc */
  override def probability(x0: Double, x1: Double): Double = implementation.probability(x0, x1)

  /** @inheritdoc */
  override def cumulativeProbability(x: Double): Double = implementation.cumulativeProbability(x)

  /** @inheritdoc */
  override def density(x: Double): Double = implementation.density(x)

  /** @inheritdoc */
  override def logDensity(x: Double): Double = implementation.logDensity(x)

  /** @inheritdoc */
  override def sample: Double = implementation.sample()

  /** @inheritdoc */
  override def toEF_Distribution: EF_Distribution = EF_Gamma(this.variable, this.shape, this.rate)
}
