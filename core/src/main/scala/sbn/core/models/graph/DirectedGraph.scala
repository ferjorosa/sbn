package sbn.core.models.graph

import sbn.core.variables.Variable

import scalax.collection.GraphEdge.DiEdge
import scalax.collection.config.CoreConfig
import scalax.collection.immutable.Graph

/**
  * This class wraps the [[Graph]] class and offers an easier interface when working with variables.
  * For a more advanced use, just access the wrapped graph.
  */
case class DirectedGraph[V <: Variable](self: Graph[V, DiEdge]) {

  /**
    * Returns the Set of [[V]] belonging to the graph.
    *
    * @return the Set of [[V]] belonging to the graph.
    */
  def nodes: Set[V] = self.nodes.toOuter

  /**
    * Returns the set of edges belonging to the graph.
    *
    * @return the set of edges belonging to the graph.
    */
  def edges: Set[DiEdge[V]] = self.edges.toOuter

  /**
    * Returns the number nodes.
    *
    * @return the number nodes.
    */
  def numberOfNodes: Int = this.nodes.size

  /**
    * Returns the number of edges.
    *
    * @return the number of edges
    */
  def numberOfEdges : Int = this.edges.size

  /**
    * Returns if the graph is acyclic of not.
    *
    * @return if the graph is acyclic of not.
    */
  def isAcyclic: Boolean = this.self.isAcyclic

  /**
    * Returns the Set of [[V]] that represent the parents of a given variable (it has incoming edges from them).
    *
    * @param variable the given variable.
    * @return the Set of [[V]] representing the parents of a given variable.
    */
  def parents(variable: V): Set[V] = this.self.get(variable).inNeighbors.map(_.value)
}

/** Factory for the [[DirectedGraph]] class. Its main factory method is created by default. */
object DirectedGraph {

  /** Implicit configuration object for the Graph builder */
  implicit val config = CoreConfig()

  /**
    * Auxiliary factory method. It is provided for an easier graph-building process.
    *
    * @param edges the set of directed edges of the graph.
    * @return a new [[DirectedGraph]] object.
    */
  def apply[V <: Variable](edges: Set[DiEdge[V]]): DirectedGraph[V] = {
    val graphBuilder = Graph.newBuilder[V, DiEdge]
    edges.map(graphBuilder += _)
    DirectedGraph(graphBuilder.result())
  }

  /**
    * Auxiliary factory method. It is provided for an easier graph-building process
    * @param variables
    * @param edges
    * @tparam V
    * @return
    */
  def apply[V <: Variable](variables: Set[V], edges: Set[DiEdge[V]]): DirectedGraph[V] = {
    val graphBuilder = Graph.newBuilder[V, DiEdge]
    variables.map(graphBuilder += _)
    edges.map(graphBuilder += _)
    DirectedGraph(graphBuilder.result())
  }
}
