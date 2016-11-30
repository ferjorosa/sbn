package sbn.core.statistics.exponentialfamily.distributions

import breeze.linalg.DenseVector
import org.apache.commons.math3.util.FastMath
import sbn.core.statistics.distributions.{Gaussian, UnivariateDistribution}
import sbn.core.variables.Variable

/**
  * Created by fer on 29/11/16.
  */
case class EF_Gaussian(variable: Variable, mean: Double, variance: Double) extends EF_UnivariateDistribution{

  override val naturalParameters: DenseVector[Double] = DenseVector(mean / variance, 1 / (2 * variance))

  override def sufficientStatistics(x: Double): DenseVector[Double] = DenseVector(x, x * x)

  override def logBaseMeasure(x: Double): Double = FastMath.log(1 / variance) - (mean * mean / (2 * variance))

  override def logNormalizer: Double = - FastMath.log(2*FastMath.PI) / 2

  override def toUnivariateDistribution: UnivariateDistribution = Gaussian(variable, mean, variance)

  // https://www.youtube.com/watch?v=ZZfOVutsIY0
  def logDensity2(x: Double): Double = naturalParameters dot sufficientStatistics(x) - (x*x / (2*variance)) - FastMath.log(2*FastMath.PI*variance)

}
