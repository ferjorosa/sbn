package sbn.core.statistics.exponentialfamily.distributions

import breeze.linalg.DenseVector
import org.apache.commons.math3.util.FastMath
import sbn.core.statistics.exponentialfamily.distributions.learning.CE_Distribution
import sbn.core.variables.MainVariable

/**
  * Created by fer on 29/11/16.
  */
trait EF_Distribution {

  val variable: MainVariable
}

trait EF_UnivariateDistribution extends EF_Distribution{

  val naturalParameters: DenseVector[Double]

  val momentParameters: DenseVector[Double]

  def zeroSufficientStatistics: DenseVector[Double]

  def sufficientStatistics(x: Double): DenseVector[Double]

  def logBaseMeasure(x: Double): Double

  def logNormalizer: Double

  def logDensity(x: Double): Double = (naturalParameters dot sufficientStatistics(x)) + logBaseMeasure(x) - logNormalizer

  def density(x: Double): Double = FastMath.exp(logDensity(x))

  def toConjugateExponentialDistribution: CE_Distribution

  def update(momentParameters: DenseVector[Double]): EF_UnivariateDistribution
}

trait EF_ConditionalDistribution extends EF_Distribution {

}
