package ferjorosa.sbn.core.distributions
import java.util.concurrent.ThreadLocalRandom

import ferjorosa.sbn.core.data.attributes.FiniteStateSpace
import ferjorosa.sbn.core.utils.Utils
import ferjorosa.sbn.core.variables.{MultinomialType, Variable}
import org.apache.commons.math3.util.FastMath

/**
  * This class represents the Multinomial distribution. This distribution can be used to compute the probabilities in situations
  * in which there are a limited set of possible outcomes. It has a parameter for each of these states. For more information
  * about this distribution, visit https://en.wikipedia.org/wiki/Multinomial_distribution).
  *
  * @param variable the associated variable.
  * @param probabilities the parameters of the distributions. Each state of the variable has an associated probability value.
  * @throws IllegalArgumentException if [[variable.distributionType]] is not [[MultinomialType]] or
  *                                  if nStates != [[probabilities.size]] or
  *                                  if [[probabilities.sum]] != 1.0
  */
@throws[IllegalArgumentException]
case class Multinomial(variable: Variable, probabilities: Vector[Double]) extends UnivariateDistribution{

  /** The state space of the multinomial variable. */
  private val variableStateSpace: FiniteStateSpace = variable.attribute.stateSpaceType match {
    case finite: FiniteStateSpace => finite
    // Note: This is a special case, technically is impossible to have a multinomial variable with a continuous state space when using its factory.
    case _ => throw new IllegalArgumentException("state space of the variable must be finite")
  }

  require(variable.distributionType.isInstanceOf[MultinomialType], "Variable must be of multinomial type")
  require(variableStateSpace.numberOfStates == probabilities.size, "One probability per state")
  require(Utils.eqDouble(probabilities.sum, 1.0), "Probabilities must sum 1.0 (sum = " + probabilities.sum + ")")

  /** @inheritdoc */
  override def label: String = "Multinomial"

  /**
    * Returns a vector containing the parameters of the distribution (the probability associated to each state).
    *
    * @return A collection of [[Double]] values corresponding to the parameters of the distribution.
    */
  override def parameters: Vector[Double] = this.probabilities

  /** @inheritdoc */
  override def numberOfParameters: Int = this.variableStateSpace.numberOfStates

  /**
    * Returns the Probability for a given state of the distribution.
    *
    * @param value a [[Double]] value representing a given state of the Multinomial distribution.
    * @throws IllegalArgumentException if value < 0 or value > nStates
    * @return the Probability for a given state of the distribution.
    */
  @throws[IllegalArgumentException]
  override def probability(value: Double): Double = try {
    this.probabilities(value.asInstanceOf[Int])
  } catch { case e: IndexOutOfBoundsException => throw new IllegalArgumentException("Invalid value")}

  /**
    * Returns the logProbability for a given state of the distribution.
    *
    * @param value a [[Double]] value representing a given state of the Multinomial distribution.
    * @throws IllegalArgumentException if value < 0 or value > numberOfParameters
    * @return the logProbability for a given state of the distribution.
    */
  @throws[IllegalArgumentException]
  override def logProbability(value: Double): Double = FastMath.log(probability(value))



  /**
    *
    * @param x
    * @param y
    * @return
    */
  override def probability(x: Double, y: Double): Double = {
    if(x > y) throw new IllegalArgumentException("Lower endpoint above upper endpoint (x > y)")

    cumulativeProbability(y) - cumulativeProbability(x)
  }

  /**
    *
    * @param value
    * @return
    */
  override def cumulativeProbability(value: Double): Double = try {
    (for (i <- 0 until value.asInstanceOf[Int]) yield probabilities(i)).sum
  } catch { case e: IndexOutOfBoundsException => throw new IllegalArgumentException("Invalid value")}

  /** @inheritdoc */
  // TODO: no tiene mucho sentido si no me equivoco
  override def density(value: Double): Double = ???

  /** @inheritdoc */
  // TODO: no tiene mucho sentido si no me equivoco
  override def logDensity(value: Double): Double = ???

  /**
    * Returns a randomly sampled value that represents an index of the variable states.
    *
    * @return a randomly sampled double value.
    */
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

  /**
    * Returns the state name of the associated multinomial variable for a given index.
    *
    * @param index the stste's index.
    * @throws IllegalArgumentException if index < 0 or index >= numberOfParameters.
    * @return the corresponding state name.
    */
  @throws[IllegalArgumentException]
  def getStateName(index: Int): String = try {
    this.variableStateSpace.stateNames(index)
  } catch { case e: IndexOutOfBoundsException => throw new IllegalArgumentException("Invalid index")}
}

/** The factory that contains specific methods for creating [[Multinomial]] objects. */
object Multinomial{

  /**
    * Factory method that produces a new Multinomial distribution with randomly created parameter values.
    *
    * @param variable the variable used to create the distribution.
    * @throws IllegalArgumentException if the variable's state space is not [[FiniteStateSpace]] or
    *                                  if the variable's distributionType is not [[MultinomialType]].
    * @return a new [[Multinomial]] distribution with randomly created parameter values.
    */
  @throws[IllegalArgumentException]
  def apply(variable: Variable): Multinomial = {
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
