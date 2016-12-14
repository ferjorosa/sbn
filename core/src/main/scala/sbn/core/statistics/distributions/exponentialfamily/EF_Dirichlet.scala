package sbn.core.statistics.distributions.exponentialfamily

import breeze.linalg.DenseVector
import sbn.core.data.attributes.FiniteStateSpace
import sbn.core.statistics.distributions.Distribution
import sbn.core.statistics.distributions.learning.CE_Distribution
import sbn.core.variables._
import sbn.core.variables.model.{DirichletType, ModelVariable}

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

  override def toDistribution: Distribution = ???

  override def generalZeroSufficientStatistics: Map[Assignments, DenseVector[Double]] = ???
}

object EF_Dirichlet {

  def apply(variable: ModelVariable, scale: Double): EF_Dirichlet = {
    require(variable.distributionType.isInstanceOf[DirichletType], "Variable must be of Dirichlet type")

    val nStates: Int = variable.attribute.stateSpaceType match {
      case finite: FiniteStateSpace => finite.numberOfStates
      case _ => throw new IllegalArgumentException("state space of the variable must be finite")
    }

    EF_Dirichlet(variable, nStates, scale)
  }

  def apply(variable: ModelVariable): EF_Dirichlet = EF_Dirichlet(variable, 2)
}
