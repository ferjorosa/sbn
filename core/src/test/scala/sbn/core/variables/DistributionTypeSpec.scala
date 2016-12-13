package sbn.core.variables

import sbn.core.CustomSpec
import sbn.core.data.attributes.{Attribute, FiniteStateSpace, RealStateSpace}
import sbn.core.statistics.distributions.{Gaussian, Gaussian_MultinomialParents, Multinomial, Multinomial_MultinomialParents}
import sbn.core.variables.model.{GaussianType, ModelVariablesFactory, MultinomialType}

class DistributionTypeSpec extends CustomSpec{

  "MultinomialType.isParentCompatible" should "return true if the parent is MultinomialType" in {

    Given("a multinomial variable ")
    val multinomialTypeChild = new MultinomialType

    When("its parent is also a multinomial variable")
    val multinomialTypeParent = new MultinomialType

    Then("they should be compatible")
    assert(multinomialTypeChild.isParentCompatible(multinomialTypeParent))
  }

  it should "return false otherwise" in {

    Given("a multinomial variable ")
    val multinomialTypeChild = new MultinomialType

    When("its parent is a gaussian variable")
    val gaussianTypeParent = new GaussianType

    Then("they should not be compatible")
    assert(!multinomialTypeChild.isParentCompatible(gaussianTypeParent))
  }

  "MultinomialType.isAttributeCompatible" should "return true if the attribute's state-space is finite" in {

    Given("an attribute whose state space is finite")
    val attribute = Attribute("attribute", FiniteStateSpace(3))

    When("the variable's distributionType is MultinomialType")
    val distributionType = new MultinomialType

    Then("they should be compatible")
    assert(distributionType.isAttributeCompatible(attribute))
  }

  it should "return false otherwise" in {

    Given("an attribute whose state space is real")
    val attribute = Attribute("attribute", RealStateSpace())

    When("the variable's distributionType is MultinomialType")
    val distributionType = new MultinomialType

    Then("they should not be compatible")
    assert(!distributionType.isAttributeCompatible(attribute))
  }

  "MultinomialType.newUnivariateDistribution" should "return a Multinomial distribution" in {

    Given("a multinomial variable")
    val multinomialVar = ModelVariablesFactory.newMultinomialLV("yolo", 2)

    When("creating an univariate distribution from its distributionType")

    Then("it should be a Multinomial distribution")
    assert(multinomialVar.distributionType.newUnivariateDistribution(multinomialVar).isInstanceOf[Multinomial])
  }

  "MultinomialType.newConditionalDistribution" should "return a Multinomial_MultinomialParents distribution if all its parents are MultinomialType" in {

    Given("a multinomial variable and a set of multinomial parents")
    val multinomialVar = ModelVariablesFactory.newMultinomialLV("gaussian",4)
    val parent1 = ModelVariablesFactory.newMultinomialLV("paren1",2)
    val parent2 = ModelVariablesFactory.newMultinomialLV("parent2", 15)

    When("creating a conditional distribution from its distributionType")
    val dist = multinomialVar.distributionType.newConditionalDistribution(multinomialVar, Set(parent1, parent2))

    Then("it should be a Multinomial_MultinomialParents distribution")
    assert(dist.isInstanceOf[Multinomial_MultinomialParents])
  }

  it should "throw an IllegalArgumentException otherwise" in {

    Given("a multinomial variable and a mixed set of multinomial and gaussian parents")
    val multinomialVar = ModelVariablesFactory.newMultinomialLV("gaussian",5)
    val parent1 = ModelVariablesFactory.newGaussianLV("parent1")
    val parent2 = ModelVariablesFactory.newMultinomialLV("parent2", 15)

    When("creating a conditional distribution from its distributionType")

    Then("a IllegalArgumentException should be thrown")
    a[IllegalArgumentException] should be thrownBy {
      val dist = multinomialVar.distributionType.newConditionalDistribution(multinomialVar, Set(parent1, parent2))
    }
  }

  "GaussianType.isParentCompatible" should "return true if the parent is MultinomialType" in {

    Given("a gaussian variable ")
    val gaussianTypeChild = new GaussianType

    When("its parent is also a multinomial variable")
    val multinomialTypeParent = new MultinomialType

    Then("they should be compatible")
    assert(gaussianTypeChild.isParentCompatible(multinomialTypeParent))
  }

  it should "return false otherwise" in {

    Given("a multinomial variable ")
    val gaussianTypeChild = new GaussianType

    When("its parent is a gaussian variable")
    val gaussianTypeParent = new GaussianType

    Then("they should not be compatible")
    assert(!gaussianTypeChild.isParentCompatible(gaussianTypeParent))
  }

  "GaussianType.isAttributeCompatible" should "return true if the attribute's state-space is real" in {

    Given("an attribute whose state space is real")
    val attribute = Attribute("attribute", RealStateSpace())

    When("the variable's distributionType is GaussianType")
    val distributionType = new GaussianType

    Then("they should be compatible")
    assert(distributionType.isAttributeCompatible(attribute))
  }

  it should "return false otherwise" in {

    Given("an attribute whose state space is finite")
    val attribute = Attribute("attribute", FiniteStateSpace(3))

    When("the variable's distributionType is GaussianType")
    val distributionType = new GaussianType

    Then("they should not be compatible")
    assert(!distributionType.isAttributeCompatible(attribute))
  }

  "GaussianType.newUnivariateDistribution" should "return a Gaussian distribution" in {

    Given("a gaussian variable")
    val gaussianVar = ModelVariablesFactory.newGaussianLV("yolo1337")

    When("creating an univariate distribution from its distributionType")
    val dist = gaussianVar.distributionType.newUnivariateDistribution(gaussianVar)

    Then("it should be a Gaussian distribution")
    assert(dist.isInstanceOf[Gaussian])
  }

  "GaussianType.newConditionalDistribution" should "return a Gaussian_MultinomialParents distribution if all its parents are MultinomialType" in {

    Given("a gaussian variable and a set of multinomial parents")
    val gaussianVar = ModelVariablesFactory.newGaussianLV("gaussian")
    val parent1 = ModelVariablesFactory.newMultinomialLV("paren1",2)
    val parent2 = ModelVariablesFactory.newMultinomialLV("parent2", 15)

    When("creating a conditional distribution from its distributionType")
    val dist = gaussianVar.distributionType.newConditionalDistribution(gaussianVar, Set(parent1, parent2))

    Then("it should be a Gaussian_MultinomialParents distribution")
    assert(dist.isInstanceOf[Gaussian_MultinomialParents])
  }

  it should "throw an IllegalArgumentException otherwise" in {

    Given("a gaussian variable and a mixed set of multinomial and gaussian parents")
    val gaussianVar = ModelVariablesFactory.newGaussianLV("gaussian")
    val parent1 = ModelVariablesFactory.newGaussianLV("parent1")
    val parent2 = ModelVariablesFactory.newMultinomialLV("parent2", 15)

    When("creating a conditional distribution from its distributionType")

    Then("a IllegalArgumentException should be thrown")
    a[IllegalArgumentException] should be thrownBy {
      val dist = gaussianVar.distributionType.newConditionalDistribution(gaussianVar, Set(parent1, parent2))
    }
  }

}
