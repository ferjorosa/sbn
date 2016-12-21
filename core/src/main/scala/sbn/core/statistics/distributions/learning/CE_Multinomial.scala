package sbn.core.statistics.distributions.learning

import sbn.core.data.attributes.FiniteStateSpace
import sbn.core.statistics.distributions.Distribution
import sbn.core.statistics.distributions.exponentialfamily.EF_UnivariateDistribution
import sbn.core.variables.model.{ModelVariable, ModelVariablesFactory, MultinomialType}

/**
  * Created by fer on 2/12/16.
  */
case class CE_Multinomial(variable: ModelVariable,
                          dirichletPrior: EF_UnivariateDistribution) extends CE_Distribution{

  require(variable.distributionType.isInstanceOf[MultinomialType])
  require(dirichletPrior.isInstanceOf[EF_Dirichlet])

  override def toDistribution: Distribution = ???
}

object CE_Multinomial{

  def apply(variable: ModelVariable): CE_Multinomial = {

    /** The state space of the multinomial variable that will be the same for the dirichlet one */
    val nStates: Int = variable.attribute.stateSpaceType match {
      case finite: FiniteStateSpace => finite.numberOfStates
      case _ => throw new IllegalArgumentException("state space of the variable must be finite")
    }

    val dirichletParameterPrior = ModelVariablesFactory.newDirichletLV(variable.name + "_dirichletPrior", nStates)
    val dirichletPrior = dirichletParameterPrior.newEFUnivariateDistribution

    CE_Multinomial(variable, dirichletPrior)
  }
}