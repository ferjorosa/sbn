package sbn.core.models.graph

import scalax.collection.GraphEdge.DiEdge
import scalax.collection.config.CoreConfig
import scalax.collection.immutable.Graph

/**
  * This class wraps the [[Graph]] class and offers an easier interface when working with directed graphs.
  * For a more advanced use, just access the wrapped graph.
  *
  * @param self the wrapped [[Graph]] object that implements its main features.
  * @tparam T the type of the graph's nodes.
  */
case class DirectedGraph[T](self: Graph[T, DiEdge]) {

  /**
    * Returns the Set of [[T]] belonging to the graph.
    *
    * @return the Set of [[T]] belonging to the graph.
    */
  val nodes: Set[T] = self.nodes.toOuter

  /**
    * Returns the set of edges belonging to the graph.
    *
    * @return the set of edges belonging to the graph.
    */
  def edges: Set[DiEdge[T]] = self.edges.toOuter

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
    * Returns the Set of [[T]] that represent the parents of a given node (it has incoming edges from them).
    *
    * @param node the given node.
    * @return the Set of [[T]] representing the parents of a given node.
    */
  def parents(node: T): Set[T] = this.self.get(node).inNeighbors.map(_.value)

  // TODO: tests & doc
  def children(node: T): Set[T] = this.self.get(node).outNeighbors.map(_.value)
}

/** Factory for the [[DirectedGraph]] class. Its main factory method is created by default. */
object DirectedGraph {

  /** Implicit configuration object for the Graph builder */
  implicit val config = CoreConfig()

  /**
    * Auxiliary factory method. It is provided for an easier graph-building process.
    *
    * @param edges the set of directed edges of the graph.
    * @tparam T the type of the graph's nodes.
    * @return a new [[DirectedGraph]] object.
    */
  def apply[T](edges: Set[DiEdge[T]]): DirectedGraph[T] = {
    val graphBuilder = Graph.newBuilder[T, DiEdge]
    edges.map(graphBuilder += _)
    DirectedGraph(graphBuilder.result())
  }

  /**
    * Auxiliary factory method. It is provided for an easier graph-building process when there are non-connected nodes
    * in the graph.
    *
    * @param nodes the collection of graph nodes.
    * @param edges the set of undirected edges of the graph.
    * @tparam T the type of the graph's nodes.
    * @return a new [[DirectedGraph]] object.
    */
  def apply[T](nodes: Set[T], edges: Set[DiEdge[T]]): DirectedGraph[T] = {
    val graphBuilder = Graph.newBuilder[T, DiEdge]
    nodes.map(graphBuilder += _)
    edges.map(graphBuilder += _)
    DirectedGraph(graphBuilder.result())
  }
}
