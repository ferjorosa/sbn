package sbn.examples.core.test

import sbn.core.io.DataFileLoader
import sbn.core.learning.parametric.{MaximumLikelihood, ParameterLearningAlgorithm}
import sbn.core.models.BayesianNetwork
import sbn.core.models.graph.DirectedGraph
import sbn.core.variables.{MainVariable, MainVariablesFactory}

import scala.util.Success
import scalax.collection.GraphEdge.DiEdge

/**
  * Created by fer on 5/12/16.
  */
object testMLE {

  def main(args: Array[String]): Unit = {

    //compoundMLEIndependent()

    compoundMLE()
  }

  private def compoundMLE() = {

    val data = DataFileLoader.loadImmutableDataSet("datasets/ferjorosaData/sprinklerData720000.arff")

    val parameterLearningAlgorithm: MaximumLikelihood = new MaximumLikelihood

    val successfulDataSet = data match {
      case Success(dataSet) => dataSet
    }

    val variables: Set[MainVariable] = successfulDataSet.attributes.map(MainVariablesFactory.newMultinomialMV(_)).toSet
    val variablesVector = variables.toVector

    val cloudy = variablesVector.find(_.name == "cloudy").get
    val sprinkler = variablesVector.find(_.name == "sprinkler").get
    val rain = variablesVector.find(_.name == "rain").get
    val wetGrass = variablesVector.find(_.name == "wetGrass").get

    val edges = Set(
      DiEdge(cloudy, sprinkler),
      DiEdge(cloudy, rain),
      DiEdge(sprinkler, wetGrass),
      DiEdge(rain, wetGrass)
    )
    // Set.empty[DiEdge[MainVariable]]
    val dag = DirectedGraph(variables, edges)

    val bn: BayesianNetwork = BayesianNetwork(dag)

    val init = System.currentTimeMillis()

    val ef_bn = parameterLearningAlgorithm.alternativeMLE(bn.toEF_BayesianNetwork, successfulDataSet)

    val efBnInit = System.currentTimeMillis()

    val bn_new = ef_bn.toBayesianNetwork

    val efBnEnd = System.currentTimeMillis()
    println("ef_bn time:" + (efBnEnd - efBnInit))

    val end = System.currentTimeMillis()
    println(end - init)

    val x=0
  }

  private def compoundMLEIndependent()= {

    val data = DataFileLoader.loadImmutableDataSet("datasets/ferjorosaData/sprinklerData300.arff")

    val parameterLearningAlgorithm: MaximumLikelihood = new MaximumLikelihood

    val successfulDataSet = data match {
      case Success(dataSet) => dataSet
    }

    val variables: Set[MainVariable] = successfulDataSet.attributes.map(MainVariablesFactory.newMultinomialMV(_)).toSet

    val dag = DirectedGraph(variables, Set.empty[DiEdge[MainVariable]])

    val bn: BayesianNetwork = BayesianNetwork(dag)

    val ef_bn = parameterLearningAlgorithm.MLE(bn.toEF_BayesianNetwork, successfulDataSet)
  }

  private def compoundMLEOneVar() = {

    val data = DataFileLoader.loadImmutableDataSet("datasets/ferjorosaData/sprinklerData300_oneVar.arff")

    val parameterLearningAlgorithm: MaximumLikelihood = new MaximumLikelihood

    val successfulDataSet = data match {
      case Success(dataSet) => dataSet
    }

    val variables: Set[MainVariable] = Set(MainVariablesFactory.newMultinomialMV(successfulDataSet.attributes.getAttributeByName("cloudy")))

    val dag = DirectedGraph(variables, Set.empty[DiEdge[MainVariable]])

    val bn: BayesianNetwork = BayesianNetwork(dag)

    val ef_bn = parameterLearningAlgorithm.MLE(bn.toEF_BayesianNetwork, successfulDataSet)

    println("Fin")
  }

  private def univariateMLE() = {

    val data = DataFileLoader.loadImmutableDataSet("datasets/ferjorosaData/sprinklerData300_oneVar.arff")

    val parameterLearningAlgorithm: ParameterLearningAlgorithm = new MaximumLikelihood

    val successfulDataSet = data match {
      case Success(dataSet) => dataSet
    }

    val variables: Set[MainVariable] = Set(MainVariablesFactory.newMultinomialMV(successfulDataSet.attributes.getAttributeByName("cloudy")))

    val dag = DirectedGraph(variables, Set.empty[DiEdge[MainVariable]])

    val bn: BayesianNetwork = BayesianNetwork(dag)

    val ef_bn = parameterLearningAlgorithm.learn(bn.toEF_BayesianNetwork, successfulDataSet)

    println("Fin")
  }

}
