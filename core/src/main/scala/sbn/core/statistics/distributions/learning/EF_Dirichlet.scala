package sbn.core.statistics.distributions.learning

import breeze.linalg.{DenseVector, sum}
import org.apache.commons.math3.special.Gamma
import org.apache.commons.math3.util.FastMath
import sbn.core.data.attributes.FiniteStateSpace
import sbn.core.statistics.distributions.Distribution
import sbn.core.statistics.distributions.exponentialfamily.EF_UnivariateDistribution
import sbn.core.variables._
import sbn.core.variables.model.{DirichletType, ModelVariable}

/**
  * This class represents a Dirichlet distribution in its exponential-family form.
  *
  * @param variable the associated variable.
  * @param concentrationParameters it is the value assigned to each category (variable's state).
  *                                For more information about them, see:
  *                                https://en.wikipedia.org/wiki/Dirichlet_distribution#The_concentration_parameter
  *                                https://en.wikipedia.org/wiki/Concentration_parameter
  *
  * @throws RuntimeException if [[variable.distributionType]] is not [[DirichletType]] or
  *                          or if variable.nStates < 2
  *                          or if a concentration parameter <= 0 .
  */
case class EF_Dirichlet(variable: ModelVariable, concentrationParameters: Vector[Double]) extends EF_UnivariateDistribution{

  /** The state space of the dirichlet variable. */
  val nStates: Int = variable.attribute.stateSpaceType match {
    case finite: FiniteStateSpace => finite.numberOfStates
    case _ => throw new IllegalArgumentException("state space of the variable must be finite")
  }

  require(variable.distributionType.isInstanceOf[DirichletType], "Variable must be of Dirichlet type")
  require(nStates >= 2, "The minimum number of variable's states is 2")
  require(!concentrationParameters.exists(_ <= 0), "concentration parameters need to be > 0")

  /** @inheritdoc */
  override val momentParameters: DenseVector[Double] = DenseVector(concentrationParameters.toArray)

  /** @inheritdoc */
  override val naturalParameters: DenseVector[Double] = momentParameters.map(x => x - 1.0)

  /** @inheritdoc */
  override val logNormalizer: Double = sum(naturalParameters.map(x => Gamma.logGamma(x))) - Gamma.logGamma(sum(naturalParameters))

  /** @inheritdoc */
  override def sufficientStatistics(x: Double): DenseVector[Double] = {
    val zeroes = zeroSufficientStatistics
    zeroes.update(x.asInstanceOf[Int], FastMath.log(x))
    zeroes
  }

  /** @inheritdoc */
  override def zeroSufficientStatistics: DenseVector[Double] = DenseVector.fill[Double](nStates, 0)

  /** @inheritdoc */
  override def generalZeroSufficientStatistics: Map[Assignments, DenseVector[Double]] =
    Map(Assignments(Set.empty[Assignment]) -> this.zeroSufficientStatistics)

  /** @inheritdoc */
  override def baseMeasure(x: Double): Double = 1

  /** @inheritdoc */
  override def update(momentParameters: DenseVector[Double]): EF_UnivariateDistribution = ???

  /** @inheritdoc */
  override def toDistribution: Distribution = ??? // no tiene sentido por el momento

  /** @inheritdoc */
  override def toConjugateExponentialDistribution: CE_Distribution = ??? // no tiene sentido
}

/** The factory that contains specific methods for creating [[EF_Dirichlet]] objects.*/
object EF_Dirichlet {

  /**
    * Factory method that produces a new [[EF_Dirichlet]] object from a variable with a scale of 2 by default.
    *
    * @param variable the distribution's variable.
    * @return a new [[EF_Dirichlet]] object from a variable with a scale of 2 by default.
    */
  //def apply(variable: ModelVariable, scale: Double): EF_Dirichlet = EF_Dirichlet(variable, 2)
}
