package ferjorosa.sbn.core.distributions

import ferjorosa.sbn.core.variables.Assignments

/**
  * Created by fer on 15/11/16.
  */
case class ParameterizedConditionalDistribution[T <: UnivariateDistribution](assignments: Assignments, univariateDistribution: T)

