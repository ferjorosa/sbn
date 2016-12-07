package sbn.core.statistics.exponentialfamily.distributions

import breeze.linalg.DenseVector
import sbn.core.data.attributes.FiniteStateSpace
import sbn.core.statistics.exponentialfamily.distributions.learning.CE_Distribution
import sbn.core.variables.{DirichletParameterType, ModelVariable, ParameterVariable, Variable}

/**
  * Created by fer on 2/12/16.
  */
//TODO hace uso de la función gamma
case class EF_Dirichlet(variable: ModelVariable, nStates: Int, scale: Double) extends EF_UnivariateDistribution{

  override val naturalParameters: DenseVector[Double] = ???

  override val momentParameters: DenseVector[Double] = ???

  override def sufficientStatistics(x: Double): DenseVector[Double] = ???

  override def zeroSufficientStatistics: DenseVector[Double] = ???

  override def logBaseMeasure(x: Double): Double = ???

  override def logNormalizer: Double = ???

  override def toConjugateExponentialDistribution: CE_Distribution = ??? // no tiene sentido

  override def update(momentParameters: DenseVector[Double]): EF_UnivariateDistribution = ???
}

object EF_Dirichlet {

  def apply(conjugatePriorVariable: ParameterVariable, scale: Double): EF_Dirichlet = {
    require(conjugatePriorVariable.parameterDistributionType.isInstanceOf[DirichletParameterType], "Variable must be of DirichletParameter type")

    val nStates: Int = conjugatePriorVariable.attribute.stateSpaceType match {
      case finite: FiniteStateSpace => finite.numberOfStates
      case _ => throw new IllegalArgumentException("state space of the variable must be finite")
    }

    EF_Dirichlet(conjugatePriorVariable, nStates, scale)
  }
}
