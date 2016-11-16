package ferjorosa.sbn.core.models.graph

import ferjorosa.sbn.core.CustomSpec
import ferjorosa.sbn.core.variables.VariableFactory

import scalax.collection.Graph

class DirectedGraphSpec extends CustomSpec{

  "DirectedGraph constructor" should "return a valid DirectedGraph object" in {
    val latent_gaussian = VariableFactory.newGaussianVariable("latent_gaussian")
    val latent_multinomial = VariableFactory.newMultinomialVariable("latent_multinomial", 2)
    //val manifest_gaussian =

    //val graph = DirectedGraph(Graph())
  }

  "DirectedGraph.apply" should "return a valid DirectedGraph object" in {

  }

  "DirectedGraph.nodes" should "return a Set[Variable] representing its nodes" in {

  }

  "DirectedGraph.edges" should "return a Set[UnDiEdge] representing its edges" in {

  }

  "DirectedGraph.numberOfNodes" should "return the correct number of nodes, which is the number of variables" in {

  }

  "DirectedGraph.numberOfEdges" should "return the correct number of edges (edges.size)" in {

  }

  "DirectedGraph.isAcyclic" should "return true if the graph is acyclic and false otherwise" in {

  }
}
