package ferjorosa.sbn.core.distributions
import java.util.concurrent.ThreadLocalRandom

import ferjorosa.sbn.core.data.attributes.FiniteStateSpace
import ferjorosa.sbn.core.utils.Utils
import ferjorosa.sbn.core.variables.{MultinomialType, Variable}

/**
  * This class represents the Multinomial distribution. This distribution can be used to compute the probabilities in situations
  * in which there are a limited set of possible outcomes. It has a parameter for each of these states. For more information
  * about this distribution, visit https://en.wikipedia.org/wiki/Multinomial_distribution).
  *
  * @param variable the associated variable.
  * @param nStates the number of possible outcomes of the variable.
  * @param probabilities the parameters of the distributions. Each state of the variable has an associated probability value.
  * @throws IllegalArgumentException if the variable is not of [[MultinomialType]] or
  *                                  if [[nStates]] != [[probabilities]].size or
  *                                  if [[probabilities.sum]] != 0
  */
@throws[IllegalArgumentException]
case class Multinomial(variable: Variable, nStates: Int, probabilities: Vector[Double]) extends UnivariateDistribution{
  require(variable.distributionType.isInstanceOf[MultinomialType], "Variable must be of multinomial type")
  require(nStates == probabilities.size, "One probability per state")
  require(probabilities.sum == 1.0, "Probabilities must sum 1.0")

  /** @inheritdoc */
  override def label: String = "Multinomial"

  /**
    * Returns a vector containing the parameters of the distribution (the probability associated to each state).
    * @return A collection of [[Double]] values corresponding to the parameters of the distribution.
    */
  override def parameters: Vector[Double] = this.probabilities

  /** @inheritdoc */
  override def numberOfParameters: Int = this.nStates

  /**
    * Returns the logProbability for a given state of the distribution.
    * @param value a [[Double]] value representing a given state of the Multinomial distribution.
    * @return the logProbability for a given state of the distribution.
    */
  override def getLogProbability(value: Double): Double = Math.log(this.probabilities(value.asInstanceOf[Int]))

  /**
    * Returns a randomly smapled value that represents an index of the variable states.
    * @return a randomly sampled double value.
    */
  override def sample: Double = {
    val randomValue = ThreadLocalRandom.current().nextDouble()
    var probability = 0.0
    for(i <- this.probabilities.indices){
      probability = probability + this.probabilities(i)
      if( probability > randomValue)
        return i
    }
    this.probabilities.length - 1
  }
}

/** The factory that contains specific methods for creating [[Multinomial]] objects. */
object Multinomial{

  /**
    * Factory method that produces a new Multinomial distribution with randomly created parameter values.
    * @param variable the variable used to create the distribution.
    * @throws IllegalArgumentException if the variable's state space is not finite.
    * @return a new Multinomial distribution with randomly created parameter values.
    */
  @throws[IllegalArgumentException]
  def apply(variable: Variable): Multinomial = {

    val nStates: Int = variable.attribute.stateSpaceType match {
      case finite: FiniteStateSpace => finite.numberOfStates
      case _ => throw new IllegalArgumentException("state space of the variable must be finite")
    }

    val threadLocalRandom = ThreadLocalRandom.current()
    val probabilities: IndexedSeq[Double] = for(i <- 0 to nStates) yield threadLocalRandom.nextDouble(100)/100
    Multinomial(variable, nStates, Utils.normalize[IndexedSeq[Double]](probabilities).toVector)
  }
}
