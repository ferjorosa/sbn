package sbn.core.statistics.exponentialfamily.distributions

import breeze.linalg.DenseVector
import org.apache.commons.math3.util.FastMath
import sbn.core.statistics.distributions.Distribution
import sbn.core.statistics.exponentialfamily.distributions.learning.CE_Distribution
import sbn.core.variables.{Assignment, Assignments}
import sbn.core.variables.model.ModelVariable

/**
  * This trait abstracts the Exponential family of distributions. This family is composed of a set of probability distributions
  * represented by a certain form. The exponential families include many of the most common distributions, including the normal,
  * exponential, gamma, chi-squared, beta, Dirichlet, Bernoulli, multinomial, Poisson, Wishart, Inverse Wishart and many others.
  * (source: https://en.wikipedia.org/wiki/Exponential_family)
  *
  * The exponential family provides a general framework for defining distributions using an alternative parametrization
  * (natural parameters) in combination with a sample statistics measure called 'sufficient statistics'.
  */
trait EF_Distribution {

  /** The main variable of the distribution */
  val variable: ModelVariable

  /**
    * Returns an equivalent [[Distribution]] object.
    *
    * @return an equivalent [[Distribution]] object.
    */
  def toDistribution: Distribution

  /**
    * Returns a Map grouping each [[Assignment]] with a vector of Double values representing its zero sufficient statistics.
    * In the case of an [[EF_UnivariateDistribution]], the Assignments will be empty, because that distribution would have no parents.
    *
    * @return a Map grouping each [[Assignment]] with a vector of Double values representing its zero sufficient statistics.
    */
  def generalZeroSufficientStatistics: Map[Assignments, DenseVector[Double]]
}

/**
  * This trait abstracts the Exponential family of univariate distributions, those that have only one variable and no parents.
  */
trait EF_UnivariateDistribution extends EF_Distribution{

  /**
    * The moment parameters are the "standard" form of the distribution parameters. They are transformed to natural parameters
    * using a specific function f(x), which differs on the distribution. For example, the moment parameters of a Multinomial
    * distributions are the probabilities associated to each state.
    */
  val momentParameters: DenseVector[Double]

  /**
    * The natural parameters are a transformation of the moment parameters used in the calculus of  the probability density function.
    *
    * It is one of the four key elements that participate in the probability density function.
    */
  val naturalParameters: DenseVector[Double]

  /**
    * Returns a Vector representing the sufficient statistic of the distribution for a specific value. For exponential families,
    * the sufficient statistic is a function of the data that fully summarizes the data x within the density function.
    * This means that, for any data sets x and y, the density value is the same if ss(x) = ss(y).
    *
    * It is one of the four key elements that participate in the probability density function.
    *
    * It is also important to note that this method will probably be used iteratively to produce a sum of sufficient statistic
    * values that will be subsequently normalized.
    *
    * @param x the corresponding value this function is applied to.
    * @return the sufficient statistics of the distribution for the corresponding value.
    */
  def sufficientStatistics(x: Double): DenseVector[Double]

  /**
    * Returns a vector full of zeroes, representing a 'void' sufficient statistics vector.
    *
    * @return a vector full of zeroes, representing a 'void' sufficient statistics vector.
    */
  def zeroSufficientStatistics: DenseVector[Double]

  /**
    * Returns the value obtained when applying the logarithm of the base used in the probability density function to a
    * specific value.
    *
    * It is one of the four key elements that participate in the probability density function.
    *
    * @param x the corresponding value this function is applied to.
    * @return the result of applying the logarithm of the base
    */
  def logBaseMeasure(x: Double): Double

  /**
    * Represents the log-normalization function used to normalize the resulting probability distribution. It is also called
    * the log.partition function and its value doesn't depend on the data but on the distribution.
    *
    * It is one of the four key elements that participate in the probability density function.
    *
    * @return the value of the log-normalizer function of the distribution.
    */
  def logNormalizer: Double

  /**
    * Returns the natural logarithm of the probability density function at the specified point x. This function is one of the
    * interesting aspects of the Exponential family of distributions, because it has the same form for all the distributions
    * that belong to it.
    *
    * @param x the point at which this function is evaluated.
    * @return the value of the normal logarithm of the density function at the point x.
    */
  def logDensity(x: Double): Double = (naturalParameters dot sufficientStatistics(x)) + logBaseMeasure(x) - logNormalizer

  /**
    * Returns the probability density function at the specified point x.
    *
    * @param x the point at which this function is evaluated.
    * @return the value of the density function at the point x.
    */
  def density(x: Double): Double = FastMath.exp(logDensity(x))

  /**
    *
    * @param momentParameters
    * @return
    */
  def update(momentParameters: DenseVector[Double]): EF_UnivariateDistribution

  /**
    *
    * @return
    */
  def toConjugateExponentialDistribution: CE_Distribution
}

trait EF_ConditionalDistribution extends EF_Distribution {

  val parents: Set[ModelVariable]

  val momentParameters: Vector[DenseVector[Double]]

  val naturalParameters: Vector[DenseVector[Double]]

  def naturalParameters(assignments: Assignments): DenseVector[Double]

  def momentParameters(assignments: Assignments): DenseVector[Double]

  def zeroSufficientStatistics: Vector[DenseVector[Double]]

  def sufficientStatistics(assignments: Assignments, x: Double): DenseVector[Double]

  def logBaseMeasure(assignments: Assignments, x: Double): Double

  def logNormalizer(assignments: Assignments): Double

  def logDensity(assignments: Assignments, x: Double): Double =
    (naturalParameters(assignments) dot sufficientStatistics(assignments, x)) + logBaseMeasure(assignments, x) - logNormalizer(assignments)

  def density(assignments: Assignments, x: Double): Double = FastMath.exp(logDensity(assignments, x))

  def getEF_UnivariateDistribution(assignments: Assignments): EF_UnivariateDistribution

  def toConjugateExponentialDistribution: CE_Distribution

  def update(momentParameters: Map[Assignments, DenseVector[Double]]): EF_ConditionalDistribution
}
