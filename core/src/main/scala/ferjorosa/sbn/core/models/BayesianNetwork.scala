package ferjorosa.sbn.core.models

import ferjorosa.sbn.core.distributions.{Distribution, Multinomial, Multinomial_MultinomialParents}
import ferjorosa.sbn.core.models.graph.DirectedGraph
import ferjorosa.sbn.core.variables.Variable

/**
  * This class represents a Bayesian network, which is a probabilistic graphical model that represents a set of random
  * variables and their conditional dependencies via a directed acyclic graph (source: Wikipedia). Each node of the graph
  * is associated with a probability distribution of the variable represented by the node.
  * @param name the name of the network.
  * @param dag the associated directed acyclic graph.
  * @param distributions the set of [[Distribution]] associated to each node of the [[dag]].
  */
case class BayesianNetwork(name: String, dag: DirectedGraph, distributions: Set[Distribution]) {
  require(dag.isAcyclic, "The directed graph of the BN has to be acyclic")

  def variables: Set[Variable] = this.dag.nodes
}

object BayesianNetwork {


  def apply(name: String, dag: DirectedGraph): BayesianNetwork = {

    val distributions: Set[Distribution] = dag.nodes.map( variable => {
      val parents = dag.parents(variable)
      if (parents.isEmpty)
        variable.newUnivariateDistribution
      else
        variable.newConditionalDistribution(parents)
    })

    BayesianNetwork(name, dag, distributions)
  }

  def apply(dag: DirectedGraph): BayesianNetwork = {
    val distributions: Set[Distribution] = dag.nodes.map( variable => {
      val parents = dag.parents(variable)
      if (parents.isEmpty)
        variable.newUnivariateDistribution
      else
        variable.newConditionalDistribution(parents)
    })

    BayesianNetwork("BayesianNetwork", dag, distributions)
  }
}
