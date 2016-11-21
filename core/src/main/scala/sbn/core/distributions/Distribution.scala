package sbn.core.distributions

import sbn.core.variables.{Assignment, Assignments, Variable}

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
    * Returns the set of variables that condition it.
    *
    * @return the set of variables that condition it.
    */
  def conditioningVariables: Set[Variable]

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
    * For a random variable X whose values are distributed according to this distribution, which is constrained to a
    * set of conditioning variables S that take specific values ([[Assignments]]), this method returns P(X = x | assignments).
    * For example, assignments could be composed of 2 conditioning variables (A, B) that take the values of A=a and B=b
    * respectively.
    *
    * In other words, this method represents the conditional probability mass function (CPMF) for this distribution
    * given the values of its conditiong variables.
    *
    * If we think of the [[ConditionalDistribution]] as a matrix of parameters (a Conditional Probability Table), the
    * assignments would represent a specific row of the CPT and x would represent a column. In the case that the
    * variable's type is Real (not finite), the probability associated to x will be 0, given the infinite number of
    * columns (infinite number of points).
    *
    * @param assignments the values assigned to the conditioning variables.
    * @param x the value of the main variable.
    * @throws IllegalArgumentException if the provided [[Assignments]] object is invalid for the distribution.
    * @return the conditional probability represented by a [[Double]] value.
    */
  @throws[IllegalArgumentException]
  def conditionalProbability(assignments: Assignments, x: Double): Double

  /**
    * For a random variable X whose values are distributed according to this distribution, which is constrained to a
    * set of conditioning variables S that take specific values ([[Assignments]]), this method returns P(X = x | assignments).
    *
    * In other words, this method represents the logarithm of the conditional probability mass function (CPMF) for this
    * distribution given the values of its conditiong variables.
    *
    * If we think of the [[ConditionalDistribution]] as a matrix of parameters (a Conditional Probability Table), the
    * assignments would represent a specific row of the CPT and x would represent a column. In the case that the
    * variable's type is Real (not finite), the probability associated to x will be 0, given the infinite number of
    * columns (infinite number of points). Therefore its logProbability will be log(0).
    *
    * @param assignments the values assigned to the conditioning variables.
    * @param x the value of the main variable.
    * @throws IllegalArgumentException if the provided [[Assignments]] object is invalid for the distribution.
    * @return the log conditional probability represented by a [[Double]] value.
    */
  @throws[IllegalArgumentException]
  def logConditionalProbability(assignments: Assignments, x: Double): Double

  /**
    * For a random variable X whose values are distributed according to this distribution, which is constrained to a
    * set of conditioning variables S that take specific values ([[Assignments]]), this method returns P(x0 < X <= x1 | assignments).
    *
    * If we think of the [[ConditionalDistribution]] as a matrix of parameters (a Conditional Probability Table), the
    * assignments would represent a specific row of the CPT and [x0, x1] would represent an interval. This method would return
    * the probability associated to that interval of values.
    *
    * @param assignments the conditioning variables and their associated values.
    * @param x0 the lower bound.
    * @param x1 the upper bound.
    * @throws IllegalArgumentException
    * @return the probability that this distribution will take a value in the interval (x0, x1], given its conditioning
    *         variables' values.
    */
  @throws[IllegalArgumentException]
  def conditionalProbability(assignments: Assignments, x0: Double, x1: Double): Double

  /**
    * For a random variable X whose values are distributed according to this distribution, which is constrained to a
    * set of conditioning variables S that take specific values ([[Assignments]]), this method returns P(X <= x | assignments).
    *
    * In other words, this method represents the conditional (cumulative) distribution function (CCDF) for this distribution
    * given the values of its conditioning variables.
    *
    * @param assignments the conditioning variables and their associated values.
    * @param x the value of the main variable that represent the point at which the CCDF is evaluated.
    * @throws IllegalArgumentException
    * @return the probability that a variable with this distribution will take a value less than or equal to x, given its set of
    *         conditioning variables.
    */
  @throws[IllegalArgumentException]
  def cumulativeConditionalProbability(assignments: Assignments, x: Double): Double

  /**
    * Returns the conditional probability density function (CPDF) of this distribution evaluated at the specified point x. This
    * distribution is conditioned by a set of variables S that take specific values [[Assignments]]).
    *
    * @param assignments the values assigned to the conditioning variables.
    * @param x the point at which the CPDF is evaluated.
    * @return the value of the conditional probability density function at point x.
    */
  def conditionalDensity(assignments: Assignments, x: Double): Double

  /**
    * Returns the conditional probability density function (CPDF) of this distribution evaluated at the specified point x. This
    * distribution is conditioned by a set of variables S that take specific values [[Assignments]]).
    *
    * @param assignments the values assigned to the conditioning variables.
    * @param x the point at which the CPDF is evaluated.
    * @return the logarithm of the value of the conditional probability density function at point x.
    */
  def logConditionalDensity(assignments: Assignments, x: Double): Double
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
    * @param x the point at which the PDF is evaluated.
    * @return the value of the probability density function at point x.
    */
  def density(x: Double): Double

  /**
    * Returns the natural logarithm of the probability density function (PDF) of this distribution evaluated at the
    * specified point x. In general, the PDF is the derivative of the [[cumulativeProbability(x: Double)]].
    * If the derivative does not exist at x, then an appropriate replacement should be returned,
    * e.g. [[Double.PositiveInfinity]], [[Double.NaN]], or the limit inferior or limit superior of the difference quotient.
    *
    * @param x the point at which the PDF is evaluated.
    * @return the logarithm of the value of the probability density function at point x.
    */
  def logDensity(x: Double): Double
}
