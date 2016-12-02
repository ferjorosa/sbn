package sbn.core.models

import sbn.core.models.graph.DirectedGraph
import sbn.core.statistics.exponentialfamily.distributions.learning.CE_Distribution
import sbn.core.variables.Variable

/**
  * Created by fer on 2/12/16.
  */
case class CE_BayesianNetwork(name: String, dag: DirectedGraph[Variable], distributions: Seq[CE_Distribution]) {

}

object CE_BayesianNetwork {

  //def apply(bayesianNetwork: BayesianNetwork): CE_BayesianNetwork = new CE_BayesianNetwork(name, dag, distributions)
}
