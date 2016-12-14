package sbn.core.statistics.distributions.exponentialfamily

import java.util.concurrent.ThreadLocalRandom

import breeze.linalg.{DenseVector, sum}
import org.apache.commons.math3.util.FastMath
import sbn.core.data.attributes.FiniteStateSpace
import sbn.core.statistics.distributions.learning.CE_Distribution
import sbn.core.statistics.distributions.{Distribution, Multinomial}
import sbn.core.utils.Utils
import sbn.core.variables._
import sbn.core.variables.model.{ModelVariable, MultinomialType}

/**
  * Created by fer on 1/12/16.
  */
// TODO: definir o revisar el uso de un vector inmutable para numeros Double
case class EF_Multinomial(variable: ModelVariable, probabilities: Vector[Double]) extends EF_UnivariateDistribution{

  /** The state space of the multinomial variable. */
  private val variableStateSpace: FiniteStateSpace = variable.attribute.stateSpaceType match {
    case finite: FiniteStateSpace => finite
    // Note: This is a special case, it is technically impossible to have a multinomial variable with a continuous state
    // space when using the VariableFactory.
    case _ => throw new IllegalArgumentException("state space of the variable must be finite")
  }

  require(variable.distributionType.isInstanceOf[MultinomialType], "Variable must be of multinomial type")
  require(variableStateSpace.numberOfStates == probabilities.size, "One probability per state")
  require(Utils.eqDouble(probabilities.sum, 1.0), "Probabilities must sum 1.0 (sum = " + probabilities.sum + ")")

  /** @inheritdoc */
  override val naturalParameters: DenseVector[Double] = DenseVector[Double] (probabilities.map(x => FastMath.log(x)).toArray)

  /** @inheritdoc */
  override val momentParameters: DenseVector[Double] = DenseVector[Double](probabilities.toArray)

  /** @inheritdoc */
  override def sufficientStatistics(x: Double): DenseVector[Double] = {
    val zeroes = DenseVector.zeros[Double](naturalParameters.activeSize)
    zeroes.update(x.asInstanceOf[Int], 1)
    zeroes
  }

  /** @inheritdoc */
  override def zeroSufficientStatistics: DenseVector[Double] = DenseVector.zeros(variableStateSpace.numberOfStates)

  /** @inheritdoc */
  override def generalZeroSufficientStatistics: Map[Assignments, DenseVector[Double]] = Map(Assignments(Set.empty[Assignment]) -> this.zeroSufficientStatistics)

  /** @inheritdoc */
  override def logBaseMeasure(x: Double): Double = 0

  /** @inheritdoc */
  override def logNormalizer: Double = FastMath.log(sum(naturalParameters.map(FastMath.exp)))

  /** @inheritdoc */
  override def update(momentParameters: DenseVector[Double]): EF_UnivariateDistribution = EF_Multinomial(this.variable, momentParameters)

  /** @inheritdoc */
  override def toDistribution: Distribution = Multinomial(this.variable, this.probabilities)

  /** @inheritdoc */
  override def toConjugateExponentialDistribution: CE_Distribution = {
    ???
    /*
    val dirichletParameter = ParameterVariablesFactory.newDirichletParameter(variable.name + "_DirichletParameter", variableStateSpace.numberOfStates)
    val dirichletDistribution = dirichletParameter.newEFUnivariateDistribution
    CE_Multinomial(variable, dirichletParameter)
    */
  }
}

object EF_Multinomial {

  def apply(variable: ModelVariable, momentParameters: DenseVector[Double]): EF_Multinomial = EF_Multinomial(variable, momentParameters.data.toVector)

  def apply(variable: ModelVariable): EF_Multinomial = {
    require(variable.distributionType.isInstanceOf[MultinomialType], "Variable must be of multinomial type")

    val nStates: Int = variable.attribute.stateSpaceType match {
      case finite: FiniteStateSpace => finite.numberOfStates
      case _ => throw new IllegalArgumentException("state space of the variable must be finite")
    }

    val threadLocalRandom = ThreadLocalRandom.current()
    val probabilities: IndexedSeq[Double] = for(i <- 0 until nStates) yield threadLocalRandom.nextDouble(100)/100
    EF_Multinomial(variable, Utils.normalize[IndexedSeq[Double]](probabilities).toVector)
  }
}
