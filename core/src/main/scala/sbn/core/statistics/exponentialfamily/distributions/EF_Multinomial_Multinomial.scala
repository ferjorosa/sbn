package sbn.core.statistics.exponentialfamily.distributions

import breeze.linalg.DenseVector
import sbn.core.statistics.distributions.{Distribution, Multinomial, Multinomial_MultinomialParents}
import sbn.core.statistics.exponentialfamily.distributions.learning.CE_Distribution
import sbn.core.variables.Assignments
import sbn.core.variables.model.{ModelVariable, MultinomialType}

/**
  * Created by Fernando on 12/6/2016.
  */
// TODO: Realmente es igual tanto para Mult_mult que para gauss_mult y este codigo se va a utilizar como base, lo unico dif es el constructor y toCE_Dist
case class EF_Multinomial_Multinomial(variable: ModelVariable,
                                      parents: Set[ModelVariable],
                                      assignedDistributions: Map[Assignments, EF_Multinomial]) extends EF_BaseDistribution_Multinomial(variable, parents, assignedDistributions){

  require(variable.distributionType.isInstanceOf[MultinomialType], "Variable must be of multinomial type")

  /** @inheritdoc */
  override def toConjugateExponentialDistribution: CE_Distribution = ???

  /** @inheritdoc */
  override def update(momentParameters: Map[Assignments, DenseVector[Double]]): EF_ConditionalDistribution = {
    EF_Multinomial_Multinomial.create(this.variable, this.parents, momentParameters)
  }

  /** @inheritdoc */
  override def toDistribution: Distribution =
    Multinomial_MultinomialParents(this.variable,
      this.parents,
      this.assignedDistributions.mapValues(_.toDistribution.asInstanceOf[Multinomial]))
}

object EF_Multinomial_Multinomial {

  def apply(distribution: Multinomial_MultinomialParents): EF_Multinomial_Multinomial = EF_Multinomial_Multinomial(
      distribution.variable,
      distribution.multinomialParents,
      distribution.assignedDistributions.map{case (assignment, dist) => (assignment, dist.toEF_Distribution.asInstanceOf[EF_Multinomial])})

  // TODO cambiar porque el tipo de momentParameters no se tiene en cuenta y da duplicado el apply
  def create(variable: ModelVariable, parents: Set[ModelVariable], momentParameters: Map[Assignments, DenseVector[Double]]): EF_Multinomial_Multinomial =
    EF_Multinomial_Multinomial(variable, parents, momentParameters.mapValues(EF_Multinomial(variable, _)))

}
