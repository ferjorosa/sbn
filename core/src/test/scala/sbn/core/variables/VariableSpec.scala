package sbn.core.variables

import java.util.UUID

import sbn.core.CustomSpec
import sbn.core.data.attributes.{Attribute, FiniteStateSpace, RealStateSpace}

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

  "VariableFactory.newMultinomialVariable" should "throw an IllegalArgumentException if its attribute's state space is not finite" in {
    val attribute = Attribute("attr", RealStateSpace())
    a[IllegalArgumentException] should be thrownBy{
      VariableFactory.newMultinomialVariable(attribute)
    }
  }

  it should "return a new multinomial ManifestVariable when a correct Attribute is provided" in {
    val attribute = Attribute("attr", FiniteStateSpace(10))
    val multinomialManifestVariable = VariableFactory.newMultinomialVariable(attribute)
  }

  it should "return a new multinomial LatentVariable when a correct configuration is provided" in {
    val multinomialLatentVariable = VariableFactory.newMultinomialVariable("multinomialLatentVariable", 4)

    assert(multinomialLatentVariable.isInstanceOf[LatentVariable])
    assert(multinomialLatentVariable.distributionType.isInstanceOf[MultinomialType])
    assert(!multinomialLatentVariable.id.equals(null))
  }

  "VariableFactory.newGaussianVariable" should "throw an IllegalArgumentException if its attribute's state space is not real" in {
    val attribute = Attribute("attr", FiniteStateSpace(2))
    a[IllegalArgumentException] should be thrownBy{
      VariableFactory.newGaussianVariable(attribute)
    }
  }

  it should "return a new gaussian ManifestVariable when a correct Attribute is provided" in {
    val attribute = Attribute("attr", RealStateSpace())
    val gaussianManifestVariable = VariableFactory.newGaussianVariable(attribute)

  }

  it should "return a new gaussian LatentVariable when a correct configuration is provided" in {
    val gaussianLatentVariable1 = VariableFactory.newGaussianVariable("gaussianLatentVariable1")
    val gaussianLatentVariable2 = VariableFactory.newGaussianVariable("gaussianLatentVariable2", 0, 1)

    assert(gaussianLatentVariable1.isInstanceOf[LatentVariable] && gaussianLatentVariable2.isInstanceOf[LatentVariable])
    assert(gaussianLatentVariable1.distributionType.isInstanceOf[GaussianType] && gaussianLatentVariable2.distributionType.isInstanceOf[GaussianType])
    assert(!gaussianLatentVariable1.id.equals(null) && !gaussianLatentVariable1.id.equals(null))
  }

}
