package ferjorosa.sbn.core.distributions

import ferjorosa.sbn.core.variables.{Assignment, Assignments}

/**
  * Created by fer on 3/11/16.
  */
trait Distribution extends Product with Serializable{

  /**
    * Returns the label of the distribution.
    *
    * @return The label of the distribution.
    */
  def label: String

  /**
    * Returns the number of parameters of the distribution.
    *
    * @return The number of parameters of the distribution.
    */
  def numberOfParameters: Int
}

/**
  * This trait defines a conditional distribution, which is a probability distribution whose values are conditioned to a
  * subset of variables.
  */
// TODO: this ConditionalDistribution correctly abstract the distributions that have multinomial parents only (Normal_Normal would be different, for example)
trait ConditionalDistribution extends Distribution{

  /**
    * Returns a collection containing the parent distributions that condition it.
    *
    * @return a collection containing the parent distributions that condition it.
    */
  def parents: Set[Distribution]

  /**
    * Returns the univariate distribution of an [[Assignment]] given a conditional distribution. If we think of the
    * [[ConditionalDistribution]] as a matrix of parameters (a Conditional Probability Table), this method would return
    * a row of this matrix, which corresponds o a [[UnivariateDistribution]].
    *
    * @param assignments the values of the conditioning variables.
    * @return a new [[UnivariateDistribution]] object associated to the [[Assignment]]
    */
  def getUnivariateDistribution(assignments: Assignments): UnivariateDistribution

  /**
    * Returns the log conditional probability of an [[Assignment]] and a specific value. If we think of the
    * [[ConditionalDistribution]] as a matrix of parameters (a Conditional Probability Table), the assignments would represent
    * a specific row of the CPT and the value would represent a column.
    *
    * @param assignments the values assigned to the conditioning variables.
    * @param value the value of the main variable.
    * @return the log conditional probability represented by a [[Double]] value.
    */
  def getLogConditionalProbability(assignments: Assignments, value: Double): Double

  /**
    * Returns the conditional probability of an [[Assignment]] and a specific value. Tf we think of the
    * [[ConditionalDistribution]] as a matrix of parameters (a Conditional Probability Table), the assignments would represent
    * a specific row of the CPT and the value would represent a column.
    *
    * @param assignments the values assigned to the conditioning variables.
    * @param value the value of the main variable.
    * @return the conditional probability represented by a [[Double]] value.
    */
  def getConditionalProbability(assignments: Assignments, value: Double): Double = Math.exp(getLogConditionalProbability(assignments, value))
}

/**
  * This trait defines a univariate distribution, which is a probability distribution of only one random variable.
  */
trait UnivariateDistribution extends Distribution{

  /**
    * Returns a randomly sampled double value.
    *
    * @return a randomly sampled double value.
    */
  def sample: Double

  /**
    * Returns the logProbability for a given input value.
    *
    * @param value the value. Depending on the univariate distribution, it will be codified differently.
    * @return the logProbability for a given input value.
    */
  def getLogProbability(value: Double): Double

  /**
    * Returns the probability for a given input value.
    *
    * @param value the value. Depending on the univariate distribution, it will be codified differently.
    * @return the probability for a given input value.
    */
  def getProbability(value: Double): Double = Math.exp(getLogProbability(value))

  /**
    * Returns a vector containing the parameters of the distribution.
    *
    * @return A collection of [[Double]] values corresponding to the parameters of the distribution.
    */
  def parameters: Vector[Double]
}
