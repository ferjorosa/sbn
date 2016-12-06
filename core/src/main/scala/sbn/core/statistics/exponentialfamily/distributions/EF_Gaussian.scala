package sbn.core.statistics.exponentialfamily.distributions

import breeze.linalg.DenseVector
import org.apache.commons.math3.util.FastMath
import sbn.core.statistics.exponentialfamily.distributions.learning.CE_Distribution
import sbn.core.variables.MainVariable

/**
  * Created by fer on 29/11/16.
  */
//mean, variance = MomentParameters con size = 2, ¿deberia cambiarse?
case class EF_Gaussian(variable: MainVariable, mean: Double, variance: Double) extends EF_UnivariateDistribution{

  override val naturalParameters: DenseVector[Double] = DenseVector(mean / variance, - 1 / (2 * variance))

  override val momentParameters: DenseVector[Double] = DenseVector[Double](mean, variance)

  override def sufficientStatistics(x: Double): DenseVector[Double] = DenseVector(x, x * x)

  override def zeroSufficientStatistics: DenseVector[Double] = DenseVector.zeros(2)

  override def logBaseMeasure(x: Double): Double = - FastMath.log(2*FastMath.PI) / 2

  override def logNormalizer: Double = FastMath.log(1 / variance) / 2 - (mean * mean / (2 * variance))

  override def toConjugateExponentialDistribution: CE_Distribution = ???

  override def update(momentParameters: DenseVector[Double]): EF_UnivariateDistribution = EF_Gaussian(this.variable, momentParameters)
}

object EF_Gaussian {

  def apply(variable: MainVariable, momentParameters: DenseVector[Double]): EF_Gaussian = EF_Gaussian(variable, momentParameters(0), momentParameters(1))
}