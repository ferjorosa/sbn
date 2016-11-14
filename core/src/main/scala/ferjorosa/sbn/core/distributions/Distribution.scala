package ferjorosa.sbn.core.distributions

import ferjorosa.sbn.core.variables.Assignment

/**
  * Created by fer on 3/11/16.
  */
trait Distribution extends Product with Serializable{

  /**
    * Returns the label of the distribution.
    * @return The label of the distribution.
    */
  def label: String

  /**
    * Returns the number of parameters of the distribution.
    * @return The number of parameters of the distribution.
    */
  def numberOfParameters: Int

  // FactoryMethod
  //def randomInitialization(threadLocalRandom: ThreadLocalRandom)

  //def getLogProbability(assignment: Assignment): Double

  //def getProbability(assignment: Assignment): Double

}

trait ConditionalDistribution extends Distribution{

  def parents: List[Distribution]

  def getUnivariateDistribution(assignment: Assignment): UnivariateDistribution

  def getLogConditionalProbability(assignment: Assignment): Double

  def getConditionalProbability(assignment: Assignment): Double = Math.exp(getLogConditionalProbability(assignment))

}

/**
  * this trait defines a univariate distribution, which is a probability distribution of only one random variable.
  */
trait UnivariateDistribution extends Distribution{

  /**
    * Returns a randomly sampled double value.
    * @return a randomly sampled double value.
    */
  def sample(): Double

  /**
    * Returns the logProbability for a given input value.
    * @param value the value. Depending on the univariate distribution, it will be codified differently.
    * @return the logProbability for a given input value.
    */
  def getLogProbability(value: Double): Double

  /**
    * Returns the probability for a given input value.
    * @param value the value. Depending on the univariate distribution, it will be codified differently.
    * @return the probability for a given input value.
    */
  def getProbability(value: Double): Double = Math.exp(getLogProbability(value))

  /**
    * Returns a vector containing the parameters of the distribution.
    * @return A collection of [[Double]] values corresponding to the parameters of the distribution.
    */
  def parameters: Vector[Double]
}
