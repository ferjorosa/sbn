package ferjorosa.sbn.core.models.graph

import ferjorosa.sbn.core.variables.Variable

import scalax.collection.GraphEdge.DiEdge
import scalax.collection.config.CoreConfig
import scalax.collection.immutable.Graph

/**
  * This class wraps the [[Graph]] class and offers an easier interface when working with variables.
  * For a more advanced use, just access the wrapped graph.
  */
case class DirectedGraph(self: Graph[Variable, DiEdge]) {

  /**
    * Returns the Set of [[Variable]] belonging to the graph.
    * @return the Set of [[Variable]] belonging to the graph.
    */
  def nodes: Set[Variable] = self.nodes.toOuter

  /**
    * Returns the set of edges belonging to the graph.
    * @return the set of edges belonging to the graph.
    */
  def edges: Set[DiEdge[Variable]] = self.edges.toOuter

  /**
    * Returns the number nodes.
    * @return the number nodes.
    */
  def numberOfNodes: Int = this.nodes.size

  /**
    * Returns the number of edges
    * @return the number of edges
    */
  def numberOfEdges : Int = this.edges.size

  /**
    * Returns if the graph is acyclic of not.
    * @return if the graph is acyclic of not.
    */
  def isAcyclic: Boolean = this.self.isAcyclic
}

/** Factory for the [[DirectedGraph]] class. Its main factory method is created by default. */
object DirectedGraph {

  /**
    * Auxiliary factory method. It is provided for an easier graph building process.
    * @param edges the set of directed edges of the graph.
    * @return a new [[DirectedGraph]] object.
    */
  def apply(edges: Set[DiEdge[Variable]]): DirectedGraph = {
    implicit val config = CoreConfig()
    val graphBuilder = Graph.newBuilder[Variable, DiEdge]
    edges.map(graphBuilder += _)
    DirectedGraph(graphBuilder.result())
  }
}
