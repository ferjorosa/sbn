package sbn.core.statistics.exponentialfamily.distributions

import breeze.linalg.DenseVector
import sbn.core.statistics.distributions.{Distribution, Gaussian, Gaussian_MultinomialParents}
import sbn.core.statistics.exponentialfamily.distributions.learning.CE_Distribution
import sbn.core.variables.Assignments
import sbn.core.variables.model.{GaussianType, ModelVariable}

/**
  * Created by fer on 7/12/16.
  */
case class EF_Gaussian_Multinomial(variable: ModelVariable,
                                   parents: Set[ModelVariable],
                                   assignedDistributions: Map[Assignments, EF_Gaussian]) extends EF_BaseDistribution_Multinomial(variable, parents, assignedDistributions) {

  require(variable.distributionType.isInstanceOf[GaussianType], "Variable must be of gaussian type")

  /** @inheritdoc */
  override def update(momentParameters: Map[Assignments, DenseVector[Double]]): EF_ConditionalDistribution =
    EF_Gaussian_Multinomial.create(this.variable, this.parents, momentParameters)

  /** @inheritdoc */
  override def toDistribution: Distribution =
    Gaussian_MultinomialParents(this.variable,
      this.parents,
      this.assignedDistributions.mapValues(_.toDistribution.asInstanceOf[Gaussian]))

  /** @inheritdoc */
  override def toConjugateExponentialDistribution: CE_Distribution = ???
}

object EF_Gaussian_Multinomial {

  def apply(distribution: Gaussian_MultinomialParents): EF_Gaussian_Multinomial = EF_Gaussian_Multinomial(
    distribution.variable,
    distribution.multinomialParents,
    distribution.assignedDistributions.map{case (assignment, dist) => (assignment, dist.toEF_Distribution.asInstanceOf[EF_Gaussian])})

  // TODO cambiar porque el tipo de momentParameters no se tiene en cuenta y da duplicado el apply
  def create(variable: ModelVariable, parents: Set[ModelVariable], momentParameters: Map[Assignments, DenseVector[Double]]): EF_Gaussian_Multinomial =
    EF_Gaussian_Multinomial(variable, parents, momentParameters.mapValues(EF_Gaussian(variable, _)))

}