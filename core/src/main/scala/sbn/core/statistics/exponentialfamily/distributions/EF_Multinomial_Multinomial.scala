package sbn.core.statistics.exponentialfamily.distributions

import breeze.linalg.DenseVector
import sbn.core.statistics.distributions.{Multinomial_MultinomialParents, Multinomial}
import sbn.core.statistics.exponentialfamily.distributions.learning.CE_Distribution
import sbn.core.variables.{Assignments, MainVariable, Variable}

/**
  * Created by Fernando on 12/6/2016.
  */
// TODO: Realmente es igual tanto para Mult_mult que para gauss_mult y este codigo se va a utilizar como base, lo unico dif es el constructor y toCE_Dist
case class EF_Multinomial_Multinomial(variable: MainVariable,
                                      multinomialParents: Set[MainVariable],
                                      parameterizedConditionalDistributions: Map[Assignments, EF_Distribution]) extends EF_ConditionalDistribution{

  override val naturalParameters: Vector[DenseVector[Double]] = ???

  override val momentParameters: Vector[DenseVector[Double]] = ???

  override def sufficientStatistics(x: Double): Vector[DenseVector[Double]] = ???

  // TODO: cambiar a forma condicional
  override def logNormalizer: Double = ???

  // TODO: cambiar a forma condicional
  override def logBaseMeasure(x: Double): Double = ???

  override def zeroSufficientStatistics: Vector[DenseVector[Double]] = ???

  override def toConjugateExponentialDistribution: CE_Distribution = ???
}

object EF_Multinomial_Multinomial {

  def apply(distribution: Multinomial_MultinomialParents): EF_Multinomial_Multinomial = EF_Multinomial_Multinomial(
      distribution.variable,
      distribution.multinomialParents,
      distribution.parameterizedConditionalDistributions.map{case (assignment, dist) => (assignment, dist.toEF_Distribution)})

}
