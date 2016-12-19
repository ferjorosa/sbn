package sbn.core.models.graph

import sbn.core.variables.Variable

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.config.CoreConfig
import scalax.collection.immutable.Graph

/**
  * This class wraps the [[Graph]] class and offers an easier interface when working with variables.
  * For a more advanced use, just access the wrapped graph.
  *
  * @param self the wrapped [[Graph]] object that implements its main features.
  * @tparam V the type of the variables that will act as nodes of the graph.
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

  private implicit val config = CoreConfig()

  /**
    * Auxiliary factory method. It is provided for an easier graph building process.
    *
    * @param edges the set of undirected edges of the graph.
    * @tparam V the type of the variables that will act as nodes of the graph.
    * @return a new [[UndirectedGraph]] object.
    */
  def apply[V <: Variable](edges: Set[UnDiEdge[V]]): UndirectedGraph[V] = {
    val graphBuilder = Graph.newBuilder[V, UnDiEdge]
    edges.map(graphBuilder += _)
    UndirectedGraph(graphBuilder.result)
  }

  /**
    * Auxiliary factory method. It is provided for an easier graph-building process when there are non-connected nodes
    * in the graph.
    *
    * @param variables the collection of graph nodes.
    * @param edges the set of undirected edges of the graph.
    * @tparam V the type of the variables that will act as nodes of the graph.
    * @return a new [[UndirectedGraph]]
    */
  def apply[V <: Variable](variables: Set[V], edges: Set[UnDiEdge[V]]): UndirectedGraph[V] = {
    val graphBuilder = Graph.newBuilder[V, UnDiEdge]
    variables.map(graphBuilder += _)
    edges.map(graphBuilder += _)
    UndirectedGraph(graphBuilder.result())
  }
}
