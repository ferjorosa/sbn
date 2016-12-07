package sbn.core.statistics.exponentialfamily.distributions
import breeze.linalg.DenseVector
import sbn.core.statistics.exponentialfamily.distributions.learning.CE_Distribution
import sbn.core.variables.ModelVariable

/**
  * Created by fer on 1/12/16.
  */
case class EF_Gamma(variable: ModelVariable) extends EF_UnivariateDistribution{

  override val naturalParameters: DenseVector[Double] = ???

  override val momentParameters: DenseVector[Double] = ???

  override def sufficientStatistics(x: Double): DenseVector[Double] = ???

  override def zeroSufficientStatistics: DenseVector[Double] = ???

  override def logBaseMeasure(x: Double): Double = ???

  override def logNormalizer: Double = ???

  override def toConjugateExponentialDistribution: CE_Distribution = ???

  override def update(momentParameters: DenseVector[Double]): EF_UnivariateDistribution = ???
}
