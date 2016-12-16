package sbn.core.variables

import java.util.UUID

import sbn.core.CustomSpec
import sbn.core.data.attributes.{Attribute, FiniteStateSpace, RealStateSpace}
import sbn.core.variables.model._

class VariableSpec extends CustomSpec{

  "ManifestVariable.constructor" should "throw an IllegalArgumentException if the attribute is incompatible" in {

    Given("an attribute with a finite state-space")
    val attribute = Attribute("attr", FiniteStateSpace(10))

    When("creating a manifest variable of gaussian type")

    Then("an IllegalArgumentException should be thrown")
    a[IllegalArgumentException] should be thrownBy {
      val variable = ManifestVariable(attribute, new GaussianType, UUID.randomUUID())
    }
  }

  "LatentVariable.constructor" should "throw an IllegalArgumentException if the attribute is incompatible" in {

    Given("an attribute with a finite state-space")
    val attribute = Attribute("attr", FiniteStateSpace(10))

    When("creating a latent variable of gaussian type")

    Then("an IllegalArgumentException should be thrown")
    a[IllegalArgumentException] should be thrownBy {
      val variable = LatentVariable(attribute, new GaussianType, UUID.randomUUID())
    }
  }

  "VariableFactory.newMultinomialMV(attribute)" should "throw an IllegalArgumentException if its attribute's state space is not finite" in {
    val attribute = Attribute("attr", RealStateSpace())
    a[IllegalArgumentException] should be thrownBy{
      ModelVariablesFactory.newMultinomialMV(attribute)
    }
  }

  it should "return a new multinomial ManifestVariable when a correct Attribute is provided" in {

    Given("an attribute with a finite state space composed of 10 states")
    val attribute = Attribute("attr", FiniteStateSpace(10))

    When("creating a multinomial manifest variable from it")

    Then("no IllegalArgumentException should be thrown")
    val multinomialManifestVariable = ModelVariablesFactory.newMultinomialMV(attribute)
  }

  "VariableFactory.newMultinomialLV" should "return a new multinomial LatentVariable when a correct configuration is provided" in {

    Given("a multinomial latent variable with 4 states")
    val multinomialLatentVariable = ModelVariablesFactory.newMultinomialLV("multinomialLatentVariable", 4)

    Then("the returned object should be an instance of the LatentVariable class")
    assert(multinomialLatentVariable.isInstanceOf[LatentVariable])

    And("its distribution type should be multinomial")
    assert(multinomialLatentVariable.distributionType.isInstanceOf[MultinomialType])

    And("it should have an ID")
    assert(!multinomialLatentVariable.id.equals(null))
  }

  "VariableFactory.newGaussianMV(attribute)" should "throw an IllegalArgumentException if its attribute's state space is not real" in {
    val attribute = Attribute("attr", FiniteStateSpace(2))
    a[IllegalArgumentException] should be thrownBy{
      ModelVariablesFactory.newGaussianMV(attribute)
    }
  }

  it should "return a new gaussian ManifestVariable when a correct Attribute is provided" in {

    Given("an attribute with an infinite real state space")
    val attribute = Attribute("attr", RealStateSpace())

    When("creating a gaussian manifest variable from it")

    Then("no IllegalArgumentException should be thrown")
    val gaussianManifestVariable = ModelVariablesFactory.newGaussianMV(attribute)
  }

  "VariableFactory.newGaussianLV(attribute)" should "return a new gaussian LatentVariable when a correct configuration is provided" in {

    Given("a gaussian LV with an infinite real state space and another with a real state space of [0, 1]")
    val gaussianLatentVariable1 = ModelVariablesFactory.newGaussianLV("gaussianLatentVariable1")
    val gaussianLatentVariable2 = ModelVariablesFactory.newGaussianLV("gaussianLatentVariable2", 0, 1)

    Then("the returned objects should be instances of the LatentVariable class")
    assert(gaussianLatentVariable1.isInstanceOf[LatentVariable] && gaussianLatentVariable2.isInstanceOf[LatentVariable])

    And("their distribution type should be gaussian")
    assert(gaussianLatentVariable1.distributionType.isInstanceOf[GaussianType] && gaussianLatentVariable2.distributionType.isInstanceOf[GaussianType])

    And("they should have an unique ID")
    assert(!gaussianLatentVariable1.id.equals(null) && !gaussianLatentVariable1.id.equals(null))
  }

  "VariableFactory.newGammaMV(attribute)" should "throw an IllegalArgumentException if its attribute's state space is not real" in {
    val attribute = Attribute("attr", FiniteStateSpace(2))
    a[IllegalArgumentException] should be thrownBy{
      ModelVariablesFactory.newGammaMV(attribute)
    }
  }

  it should "return a new gamma ManifestVariable when a correct Attribute is provided" in {

    Given("an attribute with an infinite real state space")
    val attribute = Attribute("attr", RealStateSpace())

    When("creating a gamma manifest variable from it")

    Then("no IllegalArgumentException should be thrown")
    val gammaManifestVariable = ModelVariablesFactory.newGammaMV(attribute)
  }

  "VariableFactory.newGammaLV(attribute)" should "return a new gamma LatentVariable when a correct configuration is provided" in {

    Given("a gaussian LV with an infinite real state space and another with a real state space of [0, 1]")
    val gammaLatentVariable1 = ModelVariablesFactory.newGammaLV("gammaLatentVariable1")
    val gammaLatentVariable2 = ModelVariablesFactory.newGammaLV("gammaLatentVariable2", 0, 1)

    Then("the returned objects should be instances of the LatentVariable class")
    assert(gammaLatentVariable1.isInstanceOf[LatentVariable] && gammaLatentVariable2.isInstanceOf[LatentVariable])

    And("their distribution type should be gaussian")
    assert(gammaLatentVariable1.distributionType.isInstanceOf[GammaType] && gammaLatentVariable2.distributionType.isInstanceOf[GammaType])

    And("they should have an unique ID")
    assert(!gammaLatentVariable1.id.equals(null) && !gammaLatentVariable1.id.equals(null))
  }

}
