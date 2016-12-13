package sbn.core.statistics.exponentialfamily.distributions.learning

import sbn.core.variables.model.{DirichletType, ModelVariable, MultinomialType}

/**
  * Created by fer on 2/12/16.
  */
case class CE_Multinomial(variable: ModelVariable,
                          dirichletPrior: ModelVariable,
                          distribution: CE_Distribution) extends CE_Distribution{

  require(variable.distributionType.isInstanceOf[MultinomialType])
  require(dirichletPrior.distributionType.isInstanceOf[DirichletType])
}

object CE_Multinomial{

  def apply(variable: ModelVariable, dirichletPrior: ModelVariable): CE_Multinomial = {
    CE_Multinomial(variable, dirichletPrior, null)
  }
}