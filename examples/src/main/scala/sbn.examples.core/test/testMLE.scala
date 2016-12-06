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
