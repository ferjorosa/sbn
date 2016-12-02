package sbn.core.statistics.exponentialfamily.distributions.learning

import sbn.core.variables.{DirichletParameterType, ModelVariable, MultinomialType, ParameterVariable}

/**
  * Created by fer on 2/12/16.
  */
case class CE_Multinomial(variable: ModelVariable,
                          parameterVariable: ParameterVariable) extends CE_Distribution{

  require(variable.distributionType.isInstanceOf[MultinomialType])
  require(parameterVariable.parameterDistributionType.isInstanceOf[DirichletParameterType])
}
