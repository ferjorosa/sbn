package sbn.core.statistics.distributions.exponentialfamily

import breeze.linalg.DenseVector
import org.apache.commons.math3.special.{Gamma => ApacheGamma}
import org.apache.commons.math3.util.FastMath
import sbn.core.statistics.distributions.learning.CE_Distribution
import sbn.core.statistics.distributions.{Distribution, Gamma}
import sbn.core.variables.model.{GammaType, ModelVariable}
import sbn.core.variables.{Assignment, Assignments}

/**
  * This class represents the Gamma distribution in its exponential-family form. The Gamma distribution is a very common
  * continuous probability distribution because the Exponential and Chi-squared distributions are special cases of it.
  * It is composed of two parameters and possess three different parametrizations:
  *
  * 1) With a shape (alpha) parameter and a scale (theta) parameter.
  * 2) With a shape (alpha) parameter and a rate (beta) parameter.
  * 3) With a shape (alpha) parameter and a mean (mu) parameter.
  *
  * This class uses the second parametrization, given that the Gamma distribution is used as a conjugate prior distribution
  * for various rate parameters (when constructing conjugate-exponential distributions used in bayesian learning).
  *
  * @param variable the distribution's variable.
  * @param shape the shape parameter of the distribution (more info in https://en.wikipedia.org/wiki/Shape_parameter).
  * @param rate the rate parameter of the distribution (more info in https://en.wikipedia.org/wiki/Scale_parameter#Rate_parameter).
  * @throws IllegalArgumentException if the variable is not of [[GammaType]] or
  *                                  if the shape is <= 0 or
  *                                  if the rate is <= 0
  */
@throws[IllegalArgumentException]
//TODO: create different constructors for the different parametrizations.
case class EF_Gamma(variable: ModelVariable, shape: Double, rate: Double) extends EF_UnivariateDistribution{

  require(variable.distributionType.isInstanceOf[GammaType], "Variable must be of GammaType")
  require(shape > 0, "Shape must be > 0")
  require(rate > 0, "Rate must be > 0")

  /** This is the inverse of the scale parameter. */
  val scale = 1 / rate

  /** This is the mean parameter, which belongs to the third parametrization. */
  val mean = shape / rate

  /** @inheritdoc */
  override val naturalParameters: DenseVector[Double] = DenseVector(shape - 1, - rate)

  /** @inheritdoc */
  override val momentParameters: DenseVector[Double] = DenseVector(shape, rate)

  /** @inheritdoc */
  override val logNormalizer: Double =
    ApacheGamma.logGamma(naturalParameters(0) + 1) - (naturalParameters(0) + 1) * FastMath.log(-naturalParameters(1))

  /** @inheritdoc */
  override def sufficientStatistics(x: Double): DenseVector[Double] = DenseVector(x, FastMath.log(x))

  /** @inheritdoc */
  override def zeroSufficientStatistics: DenseVector[Double] = DenseVector.zeros(2)

  /** @inheritdoc */
  override def generalZeroSufficientStatistics: Map[Assignments, DenseVector[Double]] =
    Map(Assignments(Set.empty[Assignment]) -> this.zeroSufficientStatistics)

  /** @inheritdoc */
  override def logBaseMeasure(x: Double): Double = 0

  /** @inheritdoc */
  override def update(momentParameters: DenseVector[Double]): EF_Gamma =
    EF_Gamma(this.variable, momentParameters(0), momentParameters(1))

  /** @inheritdoc */
  override def toDistribution: Distribution = Gamma(this.variable, this.shape, this.scale)

  /** @inheritdoc */
  override def toConjugateExponentialDistribution: CE_Distribution = ???

}
