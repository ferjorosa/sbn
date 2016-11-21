package sbn.core.models

import sbn.core.distributions.Distribution
import sbn.core.models.graph.DirectedGraph
import sbn.core.variables.Variable

/**
  * This class represents a Bayesian network, which is a probabilistic graphical model that represents a set of random
  * variables and their conditional dependencies via a directed acyclic graph (source: Wikipedia). Each node of the graph
  * is associated with a probability distribution of the variable represented by the node.
  *
  * @param name the name of the network.
  * @param dag the associated directed acyclic graph.
  * @param distributions the set of [[Distribution]] associated to each node of the [[dag]].
  */
case class BayesianNetwork(name: String, dag: DirectedGraph, distributions: Set[Distribution]) {
  require(dag.isAcyclic, "The directed graph of the BN has to be acyclic")

  /**
    * Returns the set of variables that consist the BN.
    *
    * @return the BN's variables.
    */
  def variables: Set[Variable] = this.dag.nodes
}

/** The factory containing specific methods for creating [[BayesianNetwork]] distribution objects */
object BayesianNetwork {

  /**
    * Factory method that produces a new Bayesian network with specifics name and DAG, but with a set of randomly
    * parameterized distributions. Each of them associated to a variable of the DAG.
    *
    * @param name the name of the network.
    * @param dag the associated directed acyclic graph.
    * @return a new [[BayesianNetwork]] with randomly parameterized distributions.
    */
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

  /**
    * Factory method that produces a new Bayesian network with a default name , a specific DAG and a set of randomly
    * parameterized distributions. Each of them associated to a variable of the DAG.
    *
    * @param dag the associated directed acyclic graph.
    * @return a new [[BayesianNetwork]] with randomly parameterized distributions.
    */
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