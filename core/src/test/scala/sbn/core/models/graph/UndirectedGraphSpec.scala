package sbn.core.models.graph

import sbn.core.CustomSpec
import sbn.core.io.DataFileLoader
import sbn.core.variables.model.{ModelVariable, ModelVariablesFactory}

import scalax.collection.GraphEdge.UnDiEdge
import scalax.collection.immutable.Graph

class UndirectedGraphSpec extends CustomSpec{

  val dataSet = DataFileLoader.loadImmutableDataSet("datasets/test/core/onlyAttributes.arff")
  val latent_gaussian = ModelVariablesFactory.newGaussianLV("latent_gaussian")
  val latent_multinomial = ModelVariablesFactory.newMultinomialLV("latent_multinomial", 2)
  val manifest_gaussian = ModelVariablesFactory.newGaussianMV(dataSet.get.attributes.getAttributeByName("continuousWithBounds"))
  val manifest_multinomial = ModelVariablesFactory.newMultinomialMV(dataSet.get.attributes.getAttributeByName("binomial"))

  val variables = Set(latent_gaussian, latent_multinomial, manifest_gaussian, manifest_multinomial)
  val edges = Set(
    UnDiEdge(latent_gaussian, manifest_gaussian),
    UnDiEdge(latent_multinomial, manifest_multinomial),
    UnDiEdge(latent_multinomial, latent_gaussian))

  private def constructAcyclicGraph: UndirectedGraph[ModelVariable] = UndirectedGraph(Graph[ModelVariable, UnDiEdge](
    UnDiEdge(latent_gaussian, manifest_gaussian),
    UnDiEdge(latent_multinomial, manifest_multinomial),
    UnDiEdge(latent_multinomial, latent_gaussian)))

  private def constructCyclicGraph: UndirectedGraph[ModelVariable] = UndirectedGraph(Graph[ModelVariable, UnDiEdge](
    UnDiEdge(latent_gaussian, manifest_gaussian),
    UnDiEdge(latent_multinomial, manifest_multinomial),
    UnDiEdge(latent_multinomial, latent_gaussian),
    UnDiEdge(manifest_gaussian, latent_multinomial)))

  "UndirectedGraph constructor" should "return a valid UndirectedGraph object" in {
    val graph = constructAcyclicGraph

    assert(graph.nodes == variables)
    assert(graph.edges.size == 3)
  }

  "UndirectedGraph.apply" should "return a valid DirectedGraph object that is equals to the constructor" in {
    val graphApply = UndirectedGraph(edges)

    val graphConstructor = constructAcyclicGraph

    assert(graphApply == graphConstructor)
    assert(graphApply equals graphConstructor)
    // Test they are not the same reference
    assert(!(graphApply eq graphConstructor))
  }

  "UndirectedGraph.nodes" should "return a Set[ModelVariable] representing its nodes" in {
    val graph = constructAcyclicGraph

    assert(graph.nodes == Set(latent_gaussian, latent_multinomial, manifest_gaussian, manifest_multinomial))
  }

  "UndirectedGraph.edges" should "return a Set[UnDiEdge] representing its edges" in {
    val graph = constructAcyclicGraph

    assert(graph.edges equals edges)
    // Test they are not the same reference
    assert(!(graph.edges eq edges))
    assert(graph.edges.isInstanceOf[Set[UnDiEdge[ModelVariable]]])
  }

  "UndirectedGraph.numberOfNodes" should "return the correct number of nodes, which is the number of variables" in {
    val graph = constructAcyclicGraph

    assert(graph.nodes.size == variables.size)
  }

  "UndirectedGraph.numberOfEdges" should "return the correct number of edges (edges.size)" in {
    val graph = constructAcyclicGraph

    assert(graph.edges.size == edges.size)
  }

  "UndirectedGraph.isAcyclic" should "return true if the graph is acyclic and false otherwise" in {
    val cyclicGraph = constructCyclicGraph
    val acyclicGraph = constructAcyclicGraph

    assert(!cyclicGraph.isAcyclic)
    assert(acyclicGraph.isAcyclic)
  }

}
