package sbn.core.models.graph

import sbn.core.variables.Variable

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.config.CoreConfig
import scalax.collection.immutable.Graph

/**
  * This class wraps the [[Graph]] class and offers an easier interface when working with variables.
  * For a more advanced use, just access the wrapped graph.
  */
case class UndirectedGraph[V <: Variable](self: Graph[V, UnDiEdge]){

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
  def edges: Set[UnDiEdge[V]] = self.edges.toOuter

  /**
    * Returns the number nodes.
    *
    * @return the number nodes.
    */
  def numberOfNodes: Int = this.nodes.size

  /**
    * Returns the number of edges.
    *
    * @return the number of edges.
    */
  def numberOfEdges : Int = this.edges.size

  /**
    * Returns if the graph is acyclic of not.
    *
    * @return if the graph is acyclic of not.
    */
  def isAcyclic: Boolean = this.self.isAcyclic
}

/** Factory for the [[UndirectedGraph]] class. Its main factory method is created by default. */
object UndirectedGraph {

  /**
    * Auxiliary factory method. It is provided for an easier graph building process.
    *
    * @param edges the set of undirected edges of the graph.
    * @return a new [[UndirectedGraph]] object.
    */
  def apply[V <: Variable](edges: Set[UnDiEdge[V]]): UndirectedGraph[V] = {
    implicit val config = CoreConfig()
    val graphBuilder = Graph.newBuilder[V, UnDiEdge]
    edges.map(graphBuilder += _)
    UndirectedGraph(graphBuilder.result)
  }
}
