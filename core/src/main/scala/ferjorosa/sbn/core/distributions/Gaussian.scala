package ferjorosa.sbn.core.distributions
import java.util.concurrent.ThreadLocalRandom

import ferjorosa.sbn.core.variables.{GaussianType, Variable}

/**
  * Created by fer on 3/11/16.
  */
@throws[IllegalArgumentException]
case class Gaussian(variable: Variable, mean: Double, variance: Double) extends UnivariateDistribution{
  require(variance > 0, "Variance > 0")
  require(variable.distributionType.isInstanceOf[GaussianType])

  def standardDeviation: Double = Math.sqrt(this.variance)

  override def sample(): Double = (ThreadLocalRandom.current().nextGaussian() * this.standardDeviation) + this.mean

  override def getLogProbability(value: Double): Double =
    -0.5 * Math.log(this.variance) - 0.5 * Math.log(2 * Math.PI) - 0.5 * Math.pow(value - this.mean, 2) / this.variance

  def getLogProbability2(value: Double): Double =
    0.5 * Math.sqrt(2 * this.variance * Math.PI) * Math.exp(- Math.pow(value - this.mean, 2) / 2 * this.variance)

  override def label: String = "Gaussian"

  override def parameters: Vector[Double] = Vector(this.mean, this.variance)

  override def numberOfParameters: Int = 2
}

object Gaussian {

  def apply(variable: Variable): Gaussian = {
    // Standard Gaussian distribution
    Gaussian(variable, 0, 1)
  }
}
