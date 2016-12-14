package sbn.core.statistics.exponentialfamily.distributions

import breeze.linalg.DenseVector
import org.apache.commons.math3.util.FastMath
import sbn.core.statistics.distributions.Distribution
import sbn.core.statistics.exponentialfamily.distributions.learning.CE_Distribution
import sbn.core.variables.{Assignment, Assignments}
import sbn.core.variables.model.ModelVariable

/**
  * This trait abstracts the Exponential family of distributions. This family is composed of a set of probability distributions
  * represented by a certain form. The exponential families include many of the most common distributions, including the Gaussian,
  * Exponential, Gamma, Chi-squared, Beta, Dirichlet, Bernoulli, Multinomial, Poisson, Wishart, Inverse Wishart and many others.
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
  * For example: Multinomial, Gamma, Gaussian, etc.
  */
trait EF_UnivariateDistribution extends EF_Distribution{

  /**
    * The moment parameters are the "standard" form of the distribution parameters. They are transformed to natural parameters
    * using a specific function f(x), which differs on the distribution. For example, the moment parameters of a Multinomial
    * distributions are the probabilities associated to each state.
    */
  val momentParameters: DenseVector[Double]

  /**
    * The natural parameters are a transformation of the moment parameters used in the calculation of  the probability
    * density function.
    *
    * It is one of the four key elements that participate in the probability density function.
    */
  val naturalParameters: DenseVector[Double]

  /**
    * Returns a vector representing the sufficient statistics of the distribution for a specific value. In the exponential
    * family, the sufficient statistic is a function of the data that fully summarizes the data x within the density function.
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
    * specific data value.
    *
    * It is one of the four key elements that participate in the probability density function.
    *
    * @param x the corresponding value this function is applied to.
    * @return the result of applying the logarithm of the base to the data x.
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
    * This method updates the [[EF_UnivariateDistribution]] with a new moment parameters vector, returning a new updated
    * distribution.
    *
    * @param momentParameters the new moment parameters vector.
    * @return a new [[EF_UnivariateDistribution]] with updated parameters.
    */
  def update(momentParameters: DenseVector[Double]): EF_UnivariateDistribution

  /**
    * Returns a new equivalent [[CE_Distribution]].
    *
    * @return a new equivalent [[CE_Distribution]].
    */
  def toConjugateExponentialDistribution: CE_Distribution
}

/**
  * This trait abstracts the Exponential family of conditional distributions, the ones whose values are conditioned by a set
  * of variables.
  */
trait EF_ConditionalDistribution extends EF_Distribution {

  /** The set of variables that condition the distribution. */
  val parents: Set[ModelVariable]

  /**
    * The moment parameters are the "standard" form of the distribution parameters. They are transformed to natural parameters
    * using a specific function f(x), which differs on the distribution. For example, the moment parameters of a Multinomial
    * distributions are the probabilities associated to each state.
    *
    * In a conditional distribution, the moment parameters are represented by a compound vector where each internal vector
    * represent the moment parameters of its associated univariate distribution.
    */
  val momentParameters: Vector[DenseVector[Double]]

  /**
    * The natural parameters are a transformation of the moment parameters used in the calculation of the probability
    * density function. Same as the moment parameters, they are represented by a compound vector where each internal vector
    * represent the natural parameters of its associated univariate distribution.
    *
    * It is one of the four key elements that participate in the probability density function.
    */
  val naturalParameters: Vector[DenseVector[Double]]

  /**
    * Returns the natural parameters vector associated with a specific set of parent assignments.
    *
    * @param assignments the [[Assignments]] object that represents the parent values.
    * @return the natural parameters vector associated with a specific set of parent assignments.
    */
  def naturalParameters(assignments: Assignments): DenseVector[Double]

  /**
    * Returns the moment parameters vector associated with a specific set of parent assignments.
    *
    * @param assignments the [[Assignments]] object that represents the parent values.
    * @return the moment parameters vector associated with a specific set of parent assignments.
    */
  def momentParameters(assignments: Assignments): DenseVector[Double]

  /**
    * Returns a compound vector where all its internal vectors are full of zeroes, representing a 'void' sufficient statistics
    * vector.
    *
    * @return a compound vector where all its internal vectors are full of zeroes.
    */
  def zeroSufficientStatistics: Vector[DenseVector[Double]]

  /**
    * Returns a vector that represents the sufficient statistics of the distribution for a specific value. In the exponential
    * family, the sufficient statistics is a function of the data that fully summarizes it within the density function.
    * This means that, for any data sets x and y, the density value is the same if ss(x) = ss(y).
    *
    * It is one of the four key elements that participate in the probability density function.
    *
    * It is also important to note that this method will probably be used iteratively to produce a sum of sufficient statistic
    * values that will be subsequently normalized.
    *
    * Given that this is a conditional distribution, the sufficient statistics function requires and additional parameter:
    * the parent values. This [[Assignments]] object will indicate these values and select the specific univariate distribution,
    * whose sufficient statistics vector we are requesting.
    *
    * @param assignments the [[Assignments]] object that represent the parent values.
    * @param x the corresponding value this function is applied to.
    * @return the sufficient statistics of the distribution for the corresponding value.
    */
  def sufficientStatistics(assignments: Assignments, x: Double): DenseVector[Double]

  /**
    * Returns the value obtained when applying the logarithm of the base used in the probability density function to a
    * specific value. Given that this is a conditional distribution
    *
    * It is one of the four key elements that participate in the probability density function.
    *
    * @param assignments the [[Assignments]] object that represents the parent values.
    * @param x the corresponding value this function is applied to.
    * @return
    */
  def logBaseMeasure(assignments: Assignments, x: Double): Double

  /**
    * Represents the log-normalization function used to normalize the resulting probability distribution. It is also called
    * the log.partition function and its value doesn't depend on the data but on the distribution.
    *
    * It is one of the four key elements that participate in the probability density function.
    *
    * @param assignments the [[Assignments]] object that represents the parent values.
    * @return the value of the log-normalizer function of the distribution.
    */
  def logNormalizer(assignments: Assignments): Double

  /**
    * Returns the natural logarithm of the probability density function at the specified point x. This function is one of the
    * interesting aspects of the Exponential family of distributions, because it has the same form for all the distributions
    * that belong to it.
    *
    * Given that this is a conditional distribution, it is necessary to pass the [[Assignments]] object that represent
    * the parent values, to select the associated univariate distribution.
    *
    * @param assignments the [[Assignments]] object that represents the parent values.
    * @param x the point at which this function is evaluated.
    * @return the value of the normal logarithm of the density function at the point x for a specific set of parent assignments.
    */
  def logDensity(assignments: Assignments, x: Double): Double =
    (naturalParameters(assignments) dot sufficientStatistics(assignments, x)) + logBaseMeasure(assignments, x) - logNormalizer(assignments)

  /**
    * Returns the probability density function at the specified point x for a specific univariate distribution, selected
    * by the [[Assignments]] object.
    *
    * @param assignments the [[Assignments]] object that represents the parent values.
    * @param x the point at which this function is evaluated.
    * @return the value of the density function at the point x.
    */
  def density(assignments: Assignments, x: Double): Double = FastMath.exp(logDensity(assignments, x))

  /**
    * Returns the [[EF_UnivariateDistribution]] associated to a set of parent values, represented by an [[Assignments]]
    * object. If we think of the [[EF_ConditionalDistribution]] as a compound vector of distributions, this method would
    * return a row of it, represented by an [[EF_UnivariateDistribution]].
    *
    * @param assignments the [[Assignments]] object that represents the parent values.
    * @return the [[EF_UnivariateDistribution]] associated to the parents' [[Assignments]].
    */
  def getEF_UnivariateDistribution(assignments: Assignments): EF_UnivariateDistribution

  /**
    * This method updates the [[EF_ConditionalDistribution]] with a new set of moment parameters vector,
    * returning a new updated distribution. Given that it is a conditional distribution, instead of passing a simple
    * vector of parameters, a Map is provided where each individual vector is associated with its corresponding
    * [[Assignments]] object. Therefore the univariate distributions associated with those [[Assignments]] objects will
    * be updated.
    *
    * @param momentParameters the Map associating each univariate distribution with its new moment parameters vector.
    * @return a new [[EF_UnivariateDistribution]] with updated parameters.
    */
  def update(momentParameters: Map[Assignments, DenseVector[Double]]): EF_ConditionalDistribution

  /**
    * Returns a new equivalent [[CE_Distribution]].
    *
    * @return a new equivalent [[CE_Distribution]].
    */
  def toConjugateExponentialDistribution: CE_Distribution
}
