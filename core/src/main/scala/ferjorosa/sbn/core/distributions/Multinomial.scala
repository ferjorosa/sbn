package ferjorosa.sbn.core.distributions
import java.util.concurrent.ThreadLocalRandom

import ferjorosa.sbn.core.data.attributes.FiniteStateSpace
import ferjorosa.sbn.core.utils.Utils

/**
  * Created by fer on 3/11/16.
  */
case class Multinomial(nStates: Int, probabilities: Vector[Double]) extends UnivariateDistribution{
  require(nStates == probabilities.size, "One probability per state")
  require(probabilities.sum == 1.0, "Probabilities must sum 1.0")

  override def label: String = "Multinomial"

  override def parameters: Vector[Double] = this.probabilities

  override def numberOfParameters: Int = this.nStates

  override def getLogProbability(value: Double): Double = Math.log(this.getProbabilityOfState(value.asInstanceOf[Int]))

  override def sample(threadLocalRandom: ThreadLocalRandom): Double = ???

  def getProbabilityOfState(state: Int): Double = this.probabilities(state)
}

object Multinomial{

  def apply(finiteStateSpace: FiniteStateSpace): Multinomial = {
    val nStates = finiteStateSpace.numberOfStates
    val probabilities = for(i <- 0 to nStates) yield 1.0/nStates
    Multinomial(nStates, probabilities.toVector)
  }

  def apply(finiteStateSpace: FiniteStateSpace, threadLocalRandom: ThreadLocalRandom): Multinomial = {
    val nStates = finiteStateSpace.numberOfStates
    val probabilities: IndexedSeq[Double] = for(i <- 0 to nStates) yield threadLocalRandom.nextDouble(100)/100
    new Multinomial(nStates, Utils.normalize[IndexedSeq[Double]](probabilities).toVector)
  }


}
