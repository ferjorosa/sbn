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
  * @throws RuntimeException if the variable is not of [[GammaType]]
  *                          or if the shape is <= 0
  *                          or if the rate is <= 0
  */
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

/** The factory containing specific methods for creating [[Gamma]] distribution objects */
object EF_Gamma {

  /**
    * Factory method that corresponds to the first possible parametrization of the Gamma distribution. This method corresponds
    * to the main constructor of the class (and its companion object's apply method).
    *
    * @param variable the distribution's variable
    * @param shape the shape parameter of the distribution (more info in https://en.wikipedia.org/wiki/Shape_parameter).
    * @param scale the scale parameter of the distribution (more info in https://en.wikipedia.org/wiki/Scale_parameter).
    * @return a new [[EF_Gamma]] distribution using the 'scale' (first) parametrization.
    */
  def createUsingScaleParameter(variable: ModelVariable, shape: Double, scale: Double): EF_Gamma =
   EF_Gamma(variable, shape, 1/ scale)

  /**
    * Factory method that corresponds to the second possible parametrization of the Gamma distribution.
    *
    * @param variable the distribution's variable
    * @param shape the shape parameter of the distribution (more info in https://en.wikipedia.org/wiki/Shape_parameter).
    * @param rate the inverse of the scale parameter (more info in https://en.wikipedia.org/wiki/Scale_parameter#Rate_parameter).
    * @return a new [[EF_Gamma]] distribution using the 'rate' (second) parametrization.
    */
  def createUsingRateParameter(variable: ModelVariable, shape: Double, rate: Double): EF_Gamma =
    EF_Gamma(variable, shape, rate)

  /**
    * Factory method that corresponds to the third possible parametrization of the Gamma distribution.
    *
    * @param variable the distribution's variable
    * @param shape the shape parameter of the distribution (more info in https://en.wikipedia.org/wiki/Shape_parameter).
    * @param mean the mean parameter of the distribution (more info in https://en.wikipedia.org/wiki/Location_parameter).
    * @return a new [[EF_Gamma]] distribution using the 'mean' (third) parametrization.
    */
  def createUsingMeanParameter(variable: ModelVariable, shape: Double, mean: Double): EF_Gamma =
    EF_Gamma(variable, shape, shape / mean)

}