package sbn.core.distributions

import sbn.core.variables.{Assignments, Variable}
import sbn.core.variables.Assignment

/**
  * This trait defines the intrinsic methods of every kind of distribution.
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
    * Returns a collection containing the parent variables that condition it.
    *
    * @return a collection containing the parent variables that condition it.
    */
  def parents: Set[Variable]

  /**
    * Returns the univariate distribution of an [[Assignment]] given a conditional distribution. If we think of the
    * [[ConditionalDistribution]] as a matrix of parameters (a Conditional Probability Table), this method would return
    * a row of this matrix, which corresponds o a [[UnivariateDistribution]].
    *
    * @param assignments the values of the conditioning variables.
    * @throws IllegalArgumentException if the provided [[Assignments]] object is invalid for the distribution.
    * @return a new [[UnivariateDistribution]] object associated to the [[Assignment]]
    */
  @throws[IllegalArgumentException]
  def getUnivariateDistribution(assignments: Assignments): UnivariateDistribution

  /**
    * Returns the log conditional probability of an [[Assignment]] and a specific value. If we think of the
    * [[ConditionalDistribution]] as a matrix of parameters (a Conditional Probability Table), the assignments would represent
    * a specific row of the CPT and the value would represent a column.
    *
    * @param assignments the values assigned to the conditioning variables.
    * @param value the value of the main variable.
    * @throws IllegalArgumentException if the provided [[Assignments]] object is invalid for the distribution.
    * @return the log conditional probability represented by a [[Double]] value.
    */
  @throws[IllegalArgumentException]
  def logConditionalProbability(assignments: Assignments, value: Double): Double

  /**
    * Returns the conditional probability of an [[Assignment]] and a specific value. Tf we think of the
    * [[ConditionalDistribution]] as a matrix of parameters (a Conditional Probability Table), the assignments would represent
    * a specific row of the CPT and the value would represent a column.
    *
    * @param assignments the values assigned to the conditioning variables.
    * @param value the value of the main variable.
    * @throws IllegalArgumentException if the provided [[Assignments]] object is invalid for the distribution.
    * @return the conditional probability represented by a [[Double]] value.
    */
  @throws[IllegalArgumentException]
  def conditionalProbability(assignments: Assignments, value: Double): Double = Math.exp(logConditionalProbability(assignments, value))
}

/**
  * This trait defines a univariate distribution, which is a probability distribution of only one random variable.
  */
trait UnivariateDistribution extends Distribution{

  /**
    * Returns a vector containing the parameters of the distribution.
    *
    * @return A collection of double values corresponding to the parameters of the distribution.
    */
  def parameters: Vector[Double]

  /**
    * Returns a randomly sampled double value.
    *
    * @return a randomly sampled double value.
    */
  def sample: Double

  /**
    * For a random variable X whose values are distributed according to this distribution, this method returns
    * P(X = x). In other  words, this method represents the probability mass function (PMF) for the distribution.
    *
    * @param x the provided point at which the PMF is evaluated.
    * @throws IllegalArgumentException if x is an invalid value.
    * @return the value of the PMF at the provided point.
    */
  @throws[IllegalArgumentException]
  def probability(x: Double): Double

  /**
    * For a random variable X whose values are distributed according to this distribution, this method returns
    * log P(X = x), where 'log' is the natural logarithm. In other words, this method represents the
    * logarithm of the probability mass function (PMF) for the distribution.
    *
    * @param x the provided point at which the PMF is evaluated.
    * @throws IllegalArgumentException if x is an invalid value.
    * @return the value of the log(PMF) at the provided point.
    */
  @throws[IllegalArgumentException]
  def logProbability(x: Double): Double

  /**
    * For a random variable X whose values are distributed according to this distribution, this method returns
    * P(x0 < X <= x1).
    *
    * @param x0 the lower bound.
    * @param x1 the upper bound.
    * @throws IllegalArgumentException if x0 > x1.
    * @return the probability that this distribution will take a value in the interval (x0, x1].
    */
  @throws[IllegalArgumentException]
  def probability(x0: Double, x1: Double): Double

  /**
    * For a random variable X whose values are distributed according to this distribution, this method returns
    * P(X <= x). In other words, this method represents the (cumulative) distribution function (CDF)
    * for this distribution.
    *
    * @param x the point at which the CDF is evaluated.
    * @return the probability that a variable with this distribution will take a value less than or equal to x.
    */
  @throws[IllegalArgumentException]
  def cumulativeProbability(x: Double): Double

  /**
    * Returns the probability density function (PDF) of this distribution evaluated at the specified point x.
    * In general, the PDF is the derivative of the [[cumulativeProbability(x: Double)]]. If the derivative does not
    * exist at x, then an appropriate replacement should be returned, e.g. [[Double.PositiveInfinity]],
    * [[Double.NaN]], or the limit inferior or limit superior of the difference quotient.
    *
    * @param x the point at which the PDF is evaluated
    * @return the value of the probability density function at point x
    */
  def density(x: Double): Double

  /**
    * Returns the natural logarithm of the probability density function (PDF) of this distribution evaluated at the
    * specified point x. In general, the PDF is the derivative of the [[cumulativeProbability(x: Double)]].
    * If the derivative does not exist at x, then an appropriate replacement should be returned,
    * e.g. [[Double.PositiveInfinity]], [[Double.NaN]], or the limit inferior or limit superior of the difference quotient.
    *
    * @param x the point at which the PDF is evaluated
    * @return the logarithm of the value of the probability density function at point x.
    */
  def logDensity(x: Double): Double
}
