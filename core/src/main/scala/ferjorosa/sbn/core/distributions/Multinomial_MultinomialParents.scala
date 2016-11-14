package ferjorosa.sbn.core.distributions
import ferjorosa.sbn.core.data.attributes.FiniteStateSpace
import ferjorosa.sbn.core.variables.{Assignment, MultinomialType, Variable}

/**
  * Created by fer on 3/11/16.
  */
case class Multinomial_MultinomialParents(variable: Variable, multinomialParents: Set[Multinomial], baseMultinomialDistributions: Set[Multinomial]) extends ConditionalDistribution {

  override def parents: List[Distribution] = this.multinomialParents.toList

  override def getUnivariateDistribution(assignment: Assignment): UnivariateDistribution = ???

  override def getLogConditionalProbability(assignment: Assignment): Double = ???

  /**
    * Returns the label of the distribution.
    *
    * @return The label of the distribution.
    */
  override def label: String = "Multinomial | Multinomial"

  /**
    * Returns the number of parameters of the distribution.
    *
    * @return The number of parameters of the distribution.
    */
  override def numberOfParameters: Int = ???

  def getMultinomial(assignment: Assignment) = ???
}

object Multinomial_MultinomialParents {

  @throws[IllegalArgumentException]
  def apply(variable: Variable, multinomialParents: Set[Variable]): Multinomial_MultinomialParents ={
    require(variable.distributionType.isInstanceOf[MultinomialType], "Variable must be of multinomial type")
    require(!multinomialParents.exists(_.distributionType.isInstanceOf[MultinomialType]), "Parents must be of multinomial type")

    val distributions = multinomialParents.flatMap{
      variable => variable.attribute.stateSpaceType match {
        case finite: FiniteStateSpace => for(i <-0 until finite.numberOfStates) yield Multinomial(variable)
        case _ => throw new IllegalArgumentException("Parents must be of multinomial type")
      }
    }
    val parentsMultinomialDistributions = multinomialParents.map(Multinomial(_))

    Multinomial_MultinomialParents(variable, parentsMultinomialDistributions, distributions)
  }
}