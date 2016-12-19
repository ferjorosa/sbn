package sbn.core.models

import sbn.core.CustomSpec
import sbn.core.io.DataFileLoader
import sbn.core.models.graph.DirectedGraph
import sbn.core.statistics.distributions.{ConditionalDistribution, Multinomial, UnivariateDistribution}
import sbn.core.variables.model.{ModelVariable, ModelVariablesFactory}

import scalax.collection.GraphEdge.DiEdge
import scalax.collection.immutable.Graph

class BayesianNetworkSpec extends CustomSpec{

  val dataSet = DataFileLoader.loadImmutableDataSet("datasets/test/core/onlyAttributes.arff")
  val latent_multinomial = ModelVariablesFactory.newMultinomialLV("latent_multinomial", 5)
  val latent_multinomial2 = ModelVariablesFactory.newMultinomialLV("latent_multinomial2", 2)
  val manifest_multinomial = ModelVariablesFactory.newMultinomialMV(dataSet.get.attributes.getAttributeByName("multinomial"))
  val manifest_multinomial2 = ModelVariablesFactory.newMultinomialMV(dataSet.get.attributes.getAttributeByName("binomial"))

  private def constructCyclicGraph: DirectedGraph[ModelVariable] = DirectedGraph(Graph[ModelVariable, DiEdge](
    DiEdge(latent_multinomial, manifest_multinomial),
    DiEdge(latent_multinomial2, manifest_multinomial2),
    DiEdge(latent_multinomial2, latent_multinomial),
    DiEdge(manifest_multinomial, latent_multinomial2)))

  private def constructAcyclicGraph: DirectedGraph[ModelVariable] = DirectedGraph(Graph[ModelVariable, DiEdge](
    DiEdge(latent_multinomial, manifest_multinomial),
    DiEdge(latent_multinomial2, manifest_multinomial2),
    DiEdge(latent_multinomial2, latent_multinomial)))

  "BayesianNetwork.constructor" should "throw a RuntimeException if the graph is not acyclic" in {

    Given("an cyclic graph composed of 4 variables and 4 edges")
    val cyclicGraph = constructCyclicGraph

    When("trying to create a Bayesian network from it with 4 distributions")
    // We create a set of false distributions (just to fulfill the size requirement)
    val distributions = cyclicGraph.nodes.map(Multinomial(_)).toSeq

    Then("a RuntimeException should be thrown")
    a[RuntimeException] should be thrownBy{
      val bn = new BayesianNetwork("bn", cyclicGraph, distributions)
    }
  }

  it should "throw a RuntimeException if the number of distributions does not coincide with the number of graph nodes" in {

    Given("an acyclic graph composed of 4 variables and 4 edges")
    val cyclicGraph = constructCyclicGraph

    When("trying to create a Bayesian network from it with a specific set of 3 distributions")
    // We create a set of false distributions (just to fulfill the size requirement)
    val distributions = cyclicGraph.nodes.map(Multinomial(_)).toSeq.drop(3) // 3 = position of the last distribution

    Then("a RuntimeException should be thrown (3 =! 4)")
    a[RuntimeException] should be thrownBy{
      val bn = new BayesianNetwork("bn", cyclicGraph, distributions)
    }
  }

  "BayesianNetwork.apply(name, acyclicGraph)" should "create a Bayesian network with the correct collection of distributions" in {

    Given("a direct acyclic graph ")
    val acyclicGraph = constructAcyclicGraph

    When("creating a Bayesian network (with a specific name) from it with a default set of distributions")
    val bn = BayesianNetwork("bn", acyclicGraph)

    Then("the BN should represent 4 distributions (1 univariate and 3 conditional)")
    assert(bn.distributions.size == 4)
    assert(bn.distributions.map(_.isInstanceOf[UnivariateDistribution]).count(_ == true) == 1)
    assert(bn.distributions.map(_.isInstanceOf[ConditionalDistribution]).count(_ == true) == 3)
  }

  "BayesianNetwork.apply(acyclicGraph)" should "create a Bayesian network with the correct collection of distributions and a default name" in {

    Given("a direct acyclic graph ")
    val acyclicGraph = constructAcyclicGraph

    When("creating a Bayesian network (with a specific name) from it with a default set of distributions")
    val bn = BayesianNetwork(acyclicGraph)

    Then("the BN should represent 4 distributions (1 univariate and 3 conditional)")
    assert(bn.distributions.size == 4)
    assert(bn.distributions.map(_.isInstanceOf[UnivariateDistribution]).count(_ == true) == 1)
    assert(bn.distributions.map(_.isInstanceOf[ConditionalDistribution]).count(_ == true) == 3)

    And("its name must be 'BayesianNetwork'")
    assert(bn.name equals "BayesianNetwork")
  }

  "BayesianNetwork.variables" should "return the set of variables that belong to the acyclic graph" in{

    Given("a direct acyclic graph ")
    val acyclicGraph = constructAcyclicGraph

    When("creating a Bayesian network from it")
    val bn = BayesianNetwork(acyclicGraph)

    Then("bn.variables should return the same set than acyclicGraph.nodes")
    assert(bn.variables equals acyclicGraph.nodes)
  }

}
