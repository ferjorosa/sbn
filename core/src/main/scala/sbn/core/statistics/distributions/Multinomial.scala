package sbn.core.statistics.distributions

import java.util.concurrent.ThreadLocalRandom

import org.apache.commons.math3.util.FastMath
import sbn.core.data.attributes.FiniteStateSpace
import sbn.core.statistics.distributions.exponentialfamily.{EF_Distribution, EF_Multinomial}
import sbn.core.statistics.distributions.learning.{CE_Distribution, CE_Multinomial}
import sbn.core.utils.Utils
import sbn.core.variables.model.{ModelVariable, MultinomialType}

/**
  * This class represents the Multinomial distribution. This distribution can be used to compute the probabilities in situations
  * in which there are a limited set of possible outcomes. It assigns a probability to each of these states. For more information
  * about this distribution, visit https://en.wikipedia.org/wiki/Multinomial_distribution).
  *
  * @param variable the associated variable.
  * @param probabilities the parameters of the distributions. Each state of the variable has an associated probability value.
  * @throws RuntimeException if [[variable.distributionType]] is not [[MultinomialType]]
  *                          or if variable.nStates != [[probabilities.size]]
  *                          or if [[probabilities.sum]] != 1.0
  */
case class Multinomial(variable: ModelVariable, probabilities: Vector[Double]) extends UnivariateDistribution{

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
  override def label: String = "Multinomial"

  /** @inheritdoc */
  override def parameters: Vector[Double] = this.probabilities

  /** @inheritdoc */
  override def numberOfParameters: Int = this.variableStateSpace.numberOfStates

  /**
    * This method represents the logarithm of the probability mass function (PMF) for the distribution. For a random
    * variable X whose values are distributed according to this distribution, this method returns log P(X = x), where x is a
    * possible state of the distribution and log represented the natural logarithm.
    *
    * @param x a double value representing a possible state of the Multinomial distribution.
    * @throws Exception if x < 0 or x > nStates
    * @return the Probability for a given state of the distribution.
    */
  override def probability(x: Double): Double = this.probabilities(x.asInstanceOf[Int])


  /**
    * This method represents the probability mass function (PMF) for the distribution. For a random variable X whose
    * values are distributed according to this distribution, this method returns P(X = x), where x is a possible state
    * of the distribution.
    *
    * @param x a double value representing a possible state of the Multinomial distribution.
    * @throws Exception if x0 > x1 or if x0, x1 are invalid.
    * @return the Probability for a given state of the distribution.
    */
  override def logProbability(x: Double): Double = FastMath.log(probability(x))

  /** @inheritdoc */
  override def probability(x0: Double, x1: Double): Double = {
    if(x0 > x1) throw new IllegalArgumentException("Lower endpoint above upper endpoint (x0 > x1)")

    cumulativeProbability(x1) - cumulativeProbability(x0)
  }

  /** @inheritdoc */
  override def cumulativeProbability(value: Double): Double = {
    if(value < 0) throw new IllegalArgumentException("Invalid value. State index has to be > 0")

    (for (i <- 0 to value.asInstanceOf[Int]) yield probabilities(i)).sum
  }

  /** @inheritdoc */
  // In a discrete distribution f(x) = P(X = x)
  override def density(x: Double): Double = probability(x)

  /** @inheritdoc */
  // In a discrete distribution f(x) = P(X = x)
  override def logDensity(x: Double): Double = logProbability(x)

  /** @inheritdoc */
  override def sample: Double = {
    // randomValue ranges [0, 1]
    val randomValue = ThreadLocalRandom.current().nextDouble()
    var probability = 0.0
    for(i <- this.probabilities.indices){
      probability = probability + this.probabilities(i)
      if( probability > randomValue)
        return i
    }
    this.probabilities.length - 1
  }

  // TODO: doc
  // TODO: no tendria sentido que se le pasasen ya las probabilidades? (similar a EF_Multinomial)
  override def toCE_Distribution: CE_Distribution = CE_Multinomial(variable)

  /** @inheritdoc */
  override def toEF_Distribution: EF_Distribution = EF_Multinomial(this.variable, this.probabilities)

  /**
    * Returns the state name of the associated multinomial variable for a given index.
    *
    * @param index the stste's index.
    * @throws Exception if index < 0 or index >= numberOfParameters.
    * @return the corresponding state name.
    */
  def getStateName(index: Int): String = this.variableStateSpace.stateNames(index)

  override def toString: String = {
    val sb: StringBuilder = new StringBuilder

    sb.append("[ ")
    parameters.foreach(x => sb.append(x + " "))
    sb.append("]")
    sb.toString()
  }
}

/** The factory that contains specific methods for creating [[Multinomial]] objects. */
object Multinomial{

  /**
    * Factory method that produces a new Multinomial distribution with randomly created parameter values.
    *
    * @param variable the variable used to create the distribution.
    * @throws RuntimeException if the variable's state space is not [[FiniteStateSpace]]
    *                          or if the variable's distributionType is not [[MultinomialType]].
    * @return a new [[Multinomial]] distribution with randomly created parameter values.
    */
  def apply(variable: ModelVariable): Multinomial = {
    require(variable.distributionType.isInstanceOf[MultinomialType], "Variable must be of multinomial type")

    val nStates: Int = variable.attribute.stateSpaceType match {
      case finite: FiniteStateSpace => finite.numberOfStates
      case _ => throw new IllegalArgumentException("state space of the variable must be finite")
    }

    val threadLocalRandom = ThreadLocalRandom.current()
    val probabilities: IndexedSeq[Double] = for(i <- 0 until nStates) yield threadLocalRandom.nextDouble(100)/100
    Multinomial(variable, Utils.normalize[IndexedSeq[Double]](probabilities).toVector)
  }
}
