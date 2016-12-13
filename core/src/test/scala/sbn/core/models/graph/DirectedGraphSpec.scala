package sbn.core.models.graph

import sbn.core.CustomSpec
import sbn.core.io.DataFileLoader
import sbn.core.variables.model.{ModelVariable, ModelVariablesFactory}

import scalax.collection.GraphEdge.DiEdge
import scalax.collection.immutable.Graph

class DirectedGraphSpec extends CustomSpec{

  val dataSet = DataFileLoader.loadImmutableDataSet("datasets/test/core/onlyAttributes.arff")
  val latent_gaussian = ModelVariablesFactory.newGaussianLV("latent_gaussian")
  val latent_multinomial = ModelVariablesFactory.newMultinomialLV("latent_multinomial", 2)
  val manifest_gaussian = ModelVariablesFactory.newGaussianMV(dataSet.get.attributes.getAttributeByName("continuousWithBounds"))
  val manifest_multinomial = ModelVariablesFactory.newMultinomialMV(dataSet.get.attributes.getAttributeByName("binomial"))

  val variables = Set(latent_gaussian, latent_multinomial, manifest_gaussian, manifest_multinomial)
  val edges = Set(
    DiEdge(latent_gaussian, manifest_gaussian),
    DiEdge(latent_multinomial, manifest_multinomial),
    DiEdge(latent_multinomial, latent_gaussian))

  private def constructAcyclicGraph: DirectedGraph[ModelVariable] = DirectedGraph(Graph[ModelVariable, DiEdge](
    DiEdge(latent_gaussian, manifest_gaussian),
    DiEdge(latent_multinomial, manifest_multinomial),
    DiEdge(latent_multinomial, latent_gaussian)))

  private def constructCyclicGraph: DirectedGraph[ModelVariable] = DirectedGraph(Graph[ModelVariable, DiEdge](
    DiEdge(latent_gaussian, manifest_gaussian),
    DiEdge(latent_multinomial, manifest_multinomial),
    DiEdge(latent_multinomial, latent_gaussian),
    DiEdge(manifest_gaussian, latent_multinomial)))


  "DirectedGraph constructor" should "return a valid DirectedGraph object" in {
    val graph = constructAcyclicGraph

    assert(graph.nodes == variables)
    assert(graph.edges.size == 3)
  }

  "DirectedGraph.apply" should "return a valid DirectedGraph object that is equals to the constructor" in {
    val graphApply = DirectedGraph(edges)

    val graphConstructor = constructAcyclicGraph

    assert(graphApply == graphConstructor)
    assert(graphApply equals graphConstructor)
    // Test they are not the same reference
    assert(!(graphApply eq graphConstructor))
  }

  "DirectedGraph.nodes" should "return a Set[ModelVariable] representing its nodes" in {
    val graph = constructAcyclicGraph

    assert(graph.nodes == Set(latent_gaussian, latent_multinomial, manifest_gaussian, manifest_multinomial))
  }

  "DirectedGraph.edges" should "return a Set[DiEdge] representing its edges" in {
    val graph = constructAcyclicGraph

    assert(graph.edges equals edges)
    // Test they are not the same reference
    assert(!(graph.edges eq edges))
    assert(graph.edges.isInstanceOf[Set[DiEdge[ModelVariable]]])
  }

  "DirectedGraph.numberOfNodes" should "return the correct number of nodes, which is the number of variables" in {
    val graph = constructAcyclicGraph

    assert(graph.nodes.size == variables.size)
  }

  "DirectedGraph.numberOfEdges" should "return the correct number of edges (edges.size)" in {
    val graph = constructAcyclicGraph

    assert(graph.edges.size == edges.size)
  }

  "DirectedGraph.isAcyclic" should "return true if the graph is acyclic and false otherwise" in {
    val cyclicGraph = constructCyclicGraph
    val acyclicGraph = constructAcyclicGraph

    assert(!cyclicGraph.isAcyclic)
    assert(acyclicGraph.isAcyclic)
  }
}
