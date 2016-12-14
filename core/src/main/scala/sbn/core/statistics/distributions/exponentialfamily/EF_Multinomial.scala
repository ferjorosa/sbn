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
  * This class represents the Multinomial distribution in its exponential-family form. This distribution can be used to
  * compute the probabilities in situations in which there are a limited set of possible outcomes. It assigns a probability
  * to each of these states. These probabilities correspond to the 'moment parameters' of the distribution. The natural
  * parameters can be obtained from them.
  *
  * @param variable the associated variable.
  * @param probabilities the parameters of the distributions. Each state of the variable has an associated probability value.
  * @throws IllegalArgumentException if [[variable.distributionType]] is not [[MultinomialType]] or
  *                                  if variable.nStates != [[probabilities.size]] or
  *                                  if [[probabilities.sum]] != 1.0
  */
@throws[IllegalArgumentException]
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
  override val logNormalizer: Double = FastMath.log(sum(naturalParameters.map(FastMath.exp)))

  /** @inheritdoc */
  override def sufficientStatistics(x: Double): DenseVector[Double] = {
    val zeroes = zeroSufficientStatistics
    zeroes.update(x.asInstanceOf[Int], 1)
    zeroes
  }

  /** @inheritdoc */
  override def zeroSufficientStatistics: DenseVector[Double] = DenseVector.zeros(variableStateSpace.numberOfStates)

  /** @inheritdoc */
  override def generalZeroSufficientStatistics: Map[Assignments, DenseVector[Double]] =
    Map(Assignments(Set.empty[Assignment]) -> this.zeroSufficientStatistics)

  /** @inheritdoc */
  override def logBaseMeasure(x: Double): Double = 0

  /** @inheritdoc */
  override def update(momentParameters: DenseVector[Double]): EF_UnivariateDistribution =
    EF_Multinomial(this.variable, momentParameters)

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

/** The factory that contains specific methods for creating [[EF_Multinomial]] objects. */
object EF_Multinomial {

  /**
    * Factory method that produces a new [[EF_Multinomial]] object from a variable and a vector of moment parameters.
    *
    * @param variable the distribution's variable.
    * @param momentParameters the moment parameters vector of the distribution.
    * @return a new object of the corresponding [[EF_Multinomial]] distribution.
    */
  def apply(variable: ModelVariable, momentParameters: DenseVector[Double]): EF_Multinomial =
    EF_Multinomial(variable, momentParameters.data.toVector)

  /**
    * Factory method that produces a new [[EF_Multinomial]] distribution with a randomly created moment parameters vector.
    *
    * @param variable the distribution's variable.
    * @throws IllegalArgumentException if the variable's state space is not [[FiniteStateSpace]] or
    *                                  if the variable's distributionType is not [[MultinomialType]].
    * @return a new [[EF_Multinomial]] distribution with a randomly created moment parameters vector.
    */
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
