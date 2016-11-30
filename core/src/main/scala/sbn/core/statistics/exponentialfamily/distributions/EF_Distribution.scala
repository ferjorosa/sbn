package sbn.core.exponentialfamily.distributions

/**
  * Created by fer on 29/11/16.
  */
trait EF_Distribution {

}

trait EF_UnivariateDistribution extends EF_Distribution{

  val naturalParameters: Double

  def sufficientStatistics(x: Double): Double

  def logBaseMeasure(x: Double): Double

  def logNormalizer: Double

  def logDensity(x: Double): Double

  def density(x: Double): Double

}

trait EF_ConditionalDistribution extends EF_Distribution {

}
