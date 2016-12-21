package sbn.core.statistics.distributions.learning

import breeze.linalg.DenseVector
import org.apache.commons.math3.special.Gamma
import org.apache.commons.math3.util.FastMath
import sbn.core.statistics.distributions.Distribution
import sbn.core.statistics.distributions.exponentialfamily.EF_UnivariateDistribution
import sbn.core.variables.{Assignment, Assignments}
import sbn.core.variables.model.ModelVariable

/**
  * Created by fer on 21/12/16.
  */
// alpha = shape
// beta = rate
// mu = mean
// lambda = variance
// TODO: realmente no es univariate, sino obviamente bivariate, pero en nuestro codigo seguimos sin distinguir entre
// multivariate, univariate, condicional...etc, aunque es verdad que a nivel de codigo nos da igual
case class EF_JointGaussianGamma(alpha: Double, beta: Double, mu: Double, lambda: Double) extends EF_UnivariateDistribution{

  val tau: Double = ???

  /** @inheritdoc */
  override val variable: ModelVariable = ???

  /** @inheritdoc */
  override val momentParameters: DenseVector[Double] = DenseVector(alpha, beta, mu, lambda)

  /** @inheritdoc */
  override val naturalParameters: DenseVector[Double] = DenseVector(alpha - 0.5, -beta - (lambda * mu * mu / 2), lambda * mu, -lambda / 2)

  /** @inheritdoc */
  override val logNormalizer: Double = Gamma.logGamma(alpha) - alpha * FastMath.log(beta) - 0.5 * FastMath.log(lambda)

  /** @inheritdoc */
  override def sufficientStatistics(x: Double): DenseVector[Double] = DenseVector(FastMath.log(tau), tau, tau * x, tau * x * x)

  /** @inheritdoc */
  override def zeroSufficientStatistics: DenseVector[Double] = DenseVector.zeros(4)

  /** @inheritdoc */
  override def generalZeroSufficientStatistics: Map[Assignments, DenseVector[Double]] =
    Map(Assignments(Set.empty[Assignment]) -> this.zeroSufficientStatistics)

  /** @inheritdoc */
  override def baseMeasure(x: Double): Double = 1 / FastMath.sqrt(2 * FastMath.PI)

  /** @inheritdoc */
  override def update(momentParameters: DenseVector[Double]): EF_UnivariateDistribution = ???

  /** @inheritdoc */
  override def toConjugateExponentialDistribution: CE_Distribution = ???

  /** @inheritdoc */
  override def toDistribution: Distribution = ???
}
