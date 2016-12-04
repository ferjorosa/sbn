package sbn.core.models

import sbn.core.models.graph.DirectedGraph
import sbn.core.statistics.distributions.Distribution
import sbn.core.variables.MainVariable

/**
  * This class represents a Bayesian network, which is a probabilistic graphical model that represents a set of random
  * variables and their conditional dependencies via a directed acyclic graph (source: Wikipedia). Each node of the graph
  * is associated with a probability distribution of the variable represented by the node.
  *
  * @param name the name of the network.
  * @param dag the associated directed acyclic graph.
  * @param distributions the seq of [[Distribution]] associated to each node of the [[dag]].
  */
// TODO: Given that the Set collection is invariant, i had to change it for Seq. Factory methods will create
// a Seq of non-repeated randomly parameterized distributions, so its "equivalent" to what i wanted to obtain with a Set (for the moment)
case class BayesianNetwork(name: String, dag: DirectedGraph[MainVariable], distributions: Seq[Distribution]) {
  require(dag.isAcyclic, "The directed graph of the BN has to be acyclic")
  require(distributions.size == dag.nodes.size, "The number of distributions must equal the number of nodes")

  /**
    * Returns the set of variables that consist the BN.
    *
    * @return the BN's variables.
    */
  def variables: Set[MainVariable] = this.dag.nodes
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
  def apply(name: String, dag: DirectedGraph[MainVariable]): BayesianNetwork = {

    val distributions: Seq[Distribution] = dag.nodes.map( variable => {
      val parents = dag.parents(variable)
      if (parents.isEmpty)
        variable.newUnivariateDistribution
      else
        variable.newConditionalDistribution(parents)
    }).toSeq

    BayesianNetwork(name, dag, distributions)
  }

  /**
    * Factory method that produces a new Bayesian network with a default name , a specific DAG and a set of randomly
    * parameterized distributions. Each of them associated to a variable of the DAG.
    *
    * @param dag the associated directed acyclic graph.
    * @return a new [[BayesianNetwork]] with randomly parameterized distributions.
    */
  def apply(dag: DirectedGraph[MainVariable]): BayesianNetwork = {
    val distributions: Seq[Distribution] = dag.nodes.map( variable => {
      val parents = dag.parents(variable)
      if (parents.isEmpty)
        variable.newUnivariateDistribution
      else
        variable.newConditionalDistribution(parents)
    }).toSeq

    BayesianNetwork("BayesianNetwork", dag, distributions)
  }
}
