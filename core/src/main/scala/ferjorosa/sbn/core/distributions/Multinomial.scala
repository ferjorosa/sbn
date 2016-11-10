package ferjorosa.sbn.core.distributions
import java.util.concurrent.ThreadLocalRandom

import ferjorosa.sbn.core.data.attributes.FiniteStateSpace
import ferjorosa.sbn.core.utils.Utils
import ferjorosa.sbn.core.variables.{MultinomialType, Variable}

/**
  * Created by fer on 3/11/16.
  */
@throws[IllegalArgumentException]
case class Multinomial(variable: Variable, nStates: Int, probabilities: Vector[Double]) extends UnivariateDistribution{
  require(variable.distributionType.isInstanceOf[MultinomialType], "Variable must be of multinomial type")
  require(nStates == probabilities.size, "One probability per state")
  require(probabilities.sum == 1.0, "Probabilities must sum 1.0")

  override def label: String = "Multinomial"

  override def parameters: Vector[Double] = this.probabilities

  override def numberOfParameters: Int = this.nStates

  override def getLogProbability(value: Double): Double = Math.log(this.getProbabilityOfState(value.asInstanceOf[Int]))

  override def sample(): Double = {
    val randomValue = ThreadLocalRandom.current().nextDouble()
    var probability = 0.0
    for(i <- this.probabilities.indices){
      probability = probability + this.probabilities(i)
      if( probability > randomValue)
        return i
    }
    this.probabilities.length - 1
  }

  def getProbabilityOfState(state: Int): Double = this.probabilities(state)
}

object Multinomial{

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
