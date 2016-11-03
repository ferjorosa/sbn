package ferjorosa.sbn.core.distributions

import java.util.concurrent.ThreadLocalRandom

import ferjorosa.sbn.core.variables.Assignment

/**
  * Created by fer on 3/11/16.
  */
trait Distribution {

  def label: String

  def parameters: Vector[Double]

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

trait UnivariateDistribution extends Distribution{

  def sample(threadLocalRandom: ThreadLocalRandom): Double

  def getLogProbability(value: Double): Double

  def getProbability(value: Double): Double = Math.exp(getLogProbability(value))
}
