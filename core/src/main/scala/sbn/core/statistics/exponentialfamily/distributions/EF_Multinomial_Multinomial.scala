package sbn.core.statistics.exponentialfamily.distributions

import breeze.linalg.DenseVector
import sbn.core.statistics.distributions.{Multinomial, Distribution, Multinomial_MultinomialParents}
import sbn.core.statistics.exponentialfamily.distributions.learning.CE_Distribution
import sbn.core.variables.{Assignments, MainVariable}

/**
  * Created by Fernando on 12/6/2016.
  */
// TODO: Realmente es igual tanto para Mult_mult que para gauss_mult y este codigo se va a utilizar como base, lo unico dif es el constructor y toCE_Dist
case class EF_Multinomial_Multinomial(variable: MainVariable,
                                      parents: Set[MainVariable],
                                      parameterizedConditionalDistributions: Map[Assignments, EF_Multinomial]) extends EF_ConditionalDistribution{

  override val naturalParameters: Vector[DenseVector[Double]] =
    parameterizedConditionalDistributions.values.map(_.naturalParameters).toVector

  override val momentParameters: Vector[DenseVector[Double]] =
    parameterizedConditionalDistributions.values.map(_.momentParameters).toVector

  override def naturalParameters(assignments: Assignments): DenseVector[Double] =
    getEF_UnivariateDistribution(assignments).naturalParameters

  override def momentParameters(assignments: Assignments): DenseVector[Double] =
    getEF_UnivariateDistribution(assignments).momentParameters

  override def sufficientStatistics(assignments: Assignments, x: Double): DenseVector[Double] =
    getEF_UnivariateDistribution(assignments).sufficientStatistics(x)

  override def zeroSufficientStatistics: Vector[DenseVector[Double]] =
    parameterizedConditionalDistributions.values.map(_.zeroSufficientStatistics).toVector

  override def logBaseMeasure(assignments: Assignments, x: Double): Double =
    getEF_UnivariateDistribution(assignments).logBaseMeasure(x)

  override def logNormalizer(assignments: Assignments): Double =
    getEF_UnivariateDistribution(assignments).logNormalizer

  /** @inheritdoc */
  override def getEF_UnivariateDistribution(assignments: Assignments): EF_UnivariateDistribution = try {
    parameterizedConditionalDistributions(assignments)
  } catch{ case nse: NoSuchElementException => throw new IllegalArgumentException("Invalid assignments for the distribution")}

  override def toConjugateExponentialDistribution: CE_Distribution = ???

  override def update(momentParameters: Map[Assignments, DenseVector[Double]]): EF_ConditionalDistribution = {
    EF_Multinomial_Multinomial.create(this.variable, this.parents, momentParameters)
  }

  override def toDistribution: Distribution =
    Multinomial_MultinomialParents(this.variable,
                                   this.parents,
                                   this.parameterizedConditionalDistributions.mapValues(_.toDistribution.asInstanceOf[Multinomial]))


  override def generalZeroSufficientStatistics: Map[Assignments, DenseVector[Double]] = parameterizedConditionalDistributions.mapValues(_.zeroSufficientStatistics)
}

object EF_Multinomial_Multinomial {

  def apply(distribution: Multinomial_MultinomialParents): EF_Multinomial_Multinomial = EF_Multinomial_Multinomial(
      distribution.variable,
      distribution.multinomialParents,
      distribution.parameterizedConditionalDistributions.map{case (assignment, dist) => (assignment, dist.toEF_Distribution.asInstanceOf[EF_Multinomial])})

  // TODO cambiar porque el tipo de momentParameters no se tiene en cuenta y da duplicado el apply
  def create(variable: MainVariable, parents: Set[MainVariable], momentParameters: Map[Assignments, DenseVector[Double]]): EF_Multinomial_Multinomial =
    EF_Multinomial_Multinomial(variable, parents, momentParameters.mapValues(EF_Multinomial(variable, _)))

}
