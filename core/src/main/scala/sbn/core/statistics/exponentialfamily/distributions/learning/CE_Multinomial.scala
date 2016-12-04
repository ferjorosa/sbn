package sbn.core.statistics.exponentialfamily.distributions.learning

import sbn.core.variables.{DirichletParameterType, MainVariable, MultinomialType, ConjugatePriorVariable}

/**
  * Created by fer on 2/12/16.
  */
case class CE_Multinomial(variable: MainVariable,
                          dirichletPrior: ConjugatePriorVariable,
                          distribution: CE_Distribution) extends CE_Distribution{

  require(variable.distributionType.isInstanceOf[MultinomialType])
  require(dirichletPrior.parameterDistributionType.isInstanceOf[DirichletParameterType])
}

object CE_Multinomial{

  def apply(variable: MainVariable, dirichletPrior: ConjugatePriorVariable): CE_Multinomial = {
    CE_Multinomial(variable, dirichletPrior, null)
  }
}