package sbn.core.models

import sbn.core.models.graph.DirectedGraph
import sbn.core.statistics.distributions.Distribution
import sbn.core.variables.model.ModelVariable

/**
  * This class represents a Bayesian network, which is a probabilistic graphical model that represents a set of random
  * variables and their conditional dependencies via a directed acyclic graph (source: Wikipedia). Each node of the graph
  * is associated with a probability distribution of the variable represented by the node.
  *
  * @param name the name of the network.
  * @param dag the associated directed acyclic graph.
  * @param distributions the seq of [[Distribution]] associated to each node of the [[dag]].
  * @throws RuntimeException if the DAG is not acyclic
  *                          or if the number of distributions doesn't match the number of nodes
  *                          or if there are distributions with repeated variables.
  */
// TODO: Given that the Set collection is invariant, i had to change it for Seq. Factory methods will create
// a Seq of non-repeated randomly parameterized distributions, so its "equivalent" to what i wanted to obtain with a Set (for the moment)
case class BayesianNetwork(name: String, dag: DirectedGraph[ModelVariable], distributions: Seq[Distribution]) {
  require(dag.isAcyclic, "The directed graph of the BN has to be acyclic")
  require(distributions.size == dag.nodes.size, "The number of distributions must equal the number of nodes")
  require(distributions.map(_.variable).distinct equals distributions.map(_.variable), "There cannot be repeated distributions")

  /**
    * Returns the set of variables that consist the BN.
    *
    * @return the BN's variables.
    */
  val variables: Set[ModelVariable] = this.dag.nodes

  /**
    * Transforms a BayesianNetwork into its Exponential Family form.
    * @return an equivalent EF_BayesianNetwork object.
    */
  def toEF_BayesianNetwork: EF_BayesianNetwork = EF_BayesianNetwork(this.name, this.dag, this.distributions.map(_.toEF_Distribution))

  //TODO: doc
  def toCE_BayesianNetwork: CE_BayesianNetwork = CE_BayesianNetwork(this.name, this.dag, this.distributions.map(_.toCE_Distribution))
}

/** The factory containing specific methods for creating [[BayesianNetwork]] distribution objects */
object BayesianNetwork {

  /**
    * Factory method that produces a new Bayesian network with specifics name and DAG, but with a set of randomly
    * parametrized distributions. Each of them associated to a variable of the DAG.
    *
    * @param name the name of the network.
    * @param dag the associated directed acyclic graph.
    * @return a new [[BayesianNetwork]] with randomly parameterized distributions.
    */
  def apply(name: String, dag: DirectedGraph[ModelVariable]): BayesianNetwork = {

    val distributions: Vector[Distribution] = dag.nodes.map( variable => {
      val parents = dag.parents(variable).toVector
      if (parents.isEmpty)
        variable.newUnivariateDistribution
      else
        variable.newConditionalDistribution(parents)
    }).toVector

    BayesianNetwork(name, dag, distributions)
  }

  /**
    * Factory method that produces a new Bayesian network with a default name , a specific DAG and a set of randomly
    * parametrized distributions. Each of them associated to a variable of the DAG.
    *
    * @param dag the associated directed acyclic graph.
    * @return a new [[BayesianNetwork]] with randomly parameterized distributions.
    */
  def apply(dag: DirectedGraph[ModelVariable]): BayesianNetwork = {
    val distributions: Vector[Distribution] = dag.nodes.map( variable => {
      val parents = dag.parents(variable).toVector
      if (parents.isEmpty)
        variable.newUnivariateDistribution
      else
        variable.newConditionalDistribution(parents)
    }).toVector

    BayesianNetwork("BayesianNetwork", dag, distributions)
  }
}
