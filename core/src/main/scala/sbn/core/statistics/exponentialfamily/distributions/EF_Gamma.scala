package sbn.core.statistics.exponentialfamily.distributions
import breeze.linalg.DenseVector
import sbn.core.statistics.exponentialfamily.distributions.learning.CE_Distribution

/**
  * Created by fer on 1/12/16.
  */
class EF_Gamma extends EF_UnivariateDistribution{

  override val naturalParameters: DenseVector[Double] = ???

  override def sufficientStatistics(x: Double): DenseVector[Double] = ???

  override def logBaseMeasure(x: Double): Double = ???

  override def logNormalizer: Double = ???

  override def toConjugateExponentialDistribution: CE_Distribution = ???
}
