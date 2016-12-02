package sbn.core.statistics.exponentialfamily.distributions
import breeze.linalg.{DenseVector, sum}
import org.apache.commons.math3.util.FastMath
import sbn.core.variables.Variable

/**
  * Created by fer on 1/12/16.
  */
case class EF_Multinomial(variable: Variable, probabilities: Vector[Double]) extends EF_UnivariateDistribution{

  override val naturalParameters: DenseVector[Double] = DenseVector[Double] (probabilities.map(x => FastMath.log(x)).toArray)

  override def sufficientStatistics(x: Double): DenseVector[Double] = {
    val zeroes = DenseVector.zeros[Double](naturalParameters.activeSize)
    zeroes.update(x.asInstanceOf[Int], 1)
    zeroes
  }

  override def logBaseMeasure(x: Double): Double = 0

  override def logNormalizer: Double = FastMath.log(sum(naturalParameters.map(FastMath.exp)))
}
