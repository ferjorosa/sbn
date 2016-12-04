package sbn.core.variables

import sbn.core.data.attributes.{Attribute, FiniteStateSpace}
import sbn.core.statistics.exponentialfamily.distributions.{EF_Dirichlet, EF_UnivariateDistribution}

/**
  * Created by fer on 2/12/16.
  */
//TODO: mirar si tiene sentido esta distinción, o al menos, distinguir bien en las distribuciones
trait ParameterDistributionType {

  /**
    * Tests whether a given attribute is compatible with the variable's distribution type. To do so, its state-space will be
    * tested against the distribution type.
    *
    * @param attribute the given attribute.
    * @return true if the attribute's state space is compatible with the distribution type, false otherwise.
    */
  def isAttributeCompatible(attribute: Attribute): Boolean

  def newEFUnivariateDistribution(parameterVariable: ConjugatePriorVariable): EF_UnivariateDistribution
}

case class DirichletParameterType() extends ParameterDistributionType {

  /** @inheritdoc */
  override def isAttributeCompatible(attribute: Attribute): Boolean = attribute.stateSpaceType match {
    case _: FiniteStateSpace => true
    case _ => false
  }

  override def newEFUnivariateDistribution(parameterVariable: ConjugatePriorVariable): EF_UnivariateDistribution = EF_Dirichlet(parameterVariable, 2)
}

