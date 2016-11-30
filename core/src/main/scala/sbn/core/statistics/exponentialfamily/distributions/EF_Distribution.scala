package sbn.core.statistics.exponentialfamily.distributions

import breeze.linalg.DenseVector
import org.apache.commons.math3.util.FastMath
import sbn.core.statistics.distributions.UnivariateDistribution

/**
  * Created by fer on 29/11/16.
  */
trait EF_Distribution {

}

trait EF_UnivariateDistribution extends EF_Distribution{

  val naturalParameters: DenseVector[Double]

  def sufficientStatistics(x: Double): DenseVector[Double]

  def logBaseMeasure(x: Double): Double

  def logNormalizer: Double

  def logDensity(x: Double): Double = (naturalParameters dot sufficientStatistics(x)) + logBaseMeasure(x) - logNormalizer

  def density(x: Double): Double = FastMath.exp(logDensity(x))

  def toUnivariateDistribution: UnivariateDistribution

}

trait EF_ConditionalDistribution extends EF_Distribution {

}
