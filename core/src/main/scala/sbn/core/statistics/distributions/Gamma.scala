package sbn.core.statistics.distributions

import org.apache.commons.math3.distribution.GammaDistribution
import org.apache.commons.math3.util.FastMath
import sbn.core.statistics.exponentialfamily.distributions.EF_Distribution
import sbn.core.variables.{GammaType, MainVariable}

/**
  * Created by fer on 1/12/16.
  */
case class Gamma(variable: MainVariable, shape: Double, scale: Double) extends UnivariateDistribution {
  require(variable.distributionType.isInstanceOf[GammaType], "Variable must be of GammaType")
  require(shape > 0, "Shape must be > 0")
  require(scale > 0, "Scale must be > 0")

  /** Apache implementation of the Gaussian (Normal) distribution */
  private val implementation: GammaDistribution = new GammaDistribution(shape, scale)

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
  override def toEF_Distribution: EF_Distribution = ???
}
