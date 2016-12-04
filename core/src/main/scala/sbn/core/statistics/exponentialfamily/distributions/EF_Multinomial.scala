package sbn.core.statistics.exponentialfamily.distributions
import breeze.linalg.{DenseVector, sum}
import org.apache.commons.math3.util.FastMath
import sbn.core.data.attributes.FiniteStateSpace
import sbn.core.statistics.exponentialfamily.distributions.learning.{CE_Distribution, CE_Multinomial}
import sbn.core.variables.{MainVariable, ParameterVariablesFactory, Variable}

/**
  * Created by fer on 1/12/16.
  */
//TODO: require variable de tipo multinomial
//TODO: revisar si es conveniente tener un parameterDistributionType
case class EF_Multinomial(variable: MainVariable, probabilities: Vector[Double]) extends EF_UnivariateDistribution{

  /** The state space of the multinomial variable. */
  private val variableStateSpace: FiniteStateSpace = variable.attribute.stateSpaceType match {
    case finite: FiniteStateSpace => finite
    // Note: This is a special case, it is technically impossible to have a multinomial variable with a continuous state
    // space when using the VariableFactory.
    case _ => throw new IllegalArgumentException("state space of the variable must be finite")
  }

  override val naturalParameters: DenseVector[Double] = DenseVector[Double] (probabilities.map(x => FastMath.log(x)).toArray)

  override def sufficientStatistics(x: Double): DenseVector[Double] = {
    val zeroes = DenseVector.zeros[Double](naturalParameters.activeSize)
    zeroes.update(x.asInstanceOf[Int], 1)
    zeroes
  }

  override def logBaseMeasure(x: Double): Double = 0

  override def logNormalizer: Double = FastMath.log(sum(naturalParameters.map(FastMath.exp)))

  override def toConjugateExponentialDistribution: CE_Distribution = {
    val dirichletParameter = ParameterVariablesFactory.newDirichletParameter(variable.name + "_DirichletParameter", variableStateSpace.numberOfStates)
    val dirichletDistribution = dirichletParameter.newEFUnivariateDistribution
    CE_Multinomial(variable, dirichletParameter)
  }
}
