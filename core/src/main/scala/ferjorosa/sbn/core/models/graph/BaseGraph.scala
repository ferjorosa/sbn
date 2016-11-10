package ferjorosa.sbn.core.models.graph

import ferjorosa.sbn.core.variables.Variable

import scalax.collection.GraphEdge.EdgeLike

/**
  * Trait that defines the main operation of the [[scalax.collection.immutable.Graph]] class wrappers provided by this library.
  */
trait BaseGraph {

  /**
    * Returns the Set of [[Variable]] belonging to the graph.
    * @return the Set of [[Variable]] belonging to the graph.
    */
  def nodes: Set[Variable]

  /**
    * Returns the set of edges belonging to the graph (edge implementation will differ).
    * @return the set of edges belonging to the graph
    */
  def edges: Set[EdgeLike[Variable]]

  /**
    * Returns the number nodes.
    * @return the number nodes.
    */
  def numberOfNodes: Int

  /**
    * Returns the number of edges
    * @return the number of edges
    */
  def numberOfEdges : Int

  /**
    * Returns if the graph is acyclic of not.
    * @return if the graph is acyclic of not.
    */
  def isAcyclic: Boolean

}
