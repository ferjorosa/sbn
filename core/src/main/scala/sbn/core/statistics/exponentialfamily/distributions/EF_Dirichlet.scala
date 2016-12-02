package sbn.core.statistics.exponentialfamily.distributions

import breeze.linalg.DenseVector
import sbn.core.data.attributes.FiniteStateSpace
import sbn.core.variables.{DirichletParameterType, ParameterVariable, Variable}

/**
  * Created by fer on 2/12/16.
  */
case class EF_Dirichlet(variable: Variable, nStates: Int, scale: Double) extends EF_UnivariateDistribution{

  override val naturalParameters: DenseVector[Double] = ???

  override def sufficientStatistics(x: Double): DenseVector[Double] = ???

  override def logBaseMeasure(x: Double): Double = ???

  override def logNormalizer: Double = ???
}

object EF_Dirichlet {

  def apply(parameterVariable: ParameterVariable, scale: Double): EF_Dirichlet = {
    require(parameterVariable.parameterDistributionType.isInstanceOf[DirichletParameterType], "Variable must be of multinomial type")

    val nStates: Int = parameterVariable.attribute.stateSpaceType match {
      case finite: FiniteStateSpace => finite.numberOfStates
      case _ => throw new IllegalArgumentException("state space of the variable must be finite")
    }

    EF_Dirichlet(parameterVariable, nStates, scale)
  }
}
