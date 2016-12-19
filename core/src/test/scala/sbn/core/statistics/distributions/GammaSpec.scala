package sbn.core.statistics.distributions

import sbn.core.CustomSpec
import sbn.core.utils.Utils
import sbn.core.variables.model.ModelVariablesFactory

/**
  * Created by Fernando on 12/16/2016.
  */
// TODO: All the probability, density, etc methods (not easy to check, I can trust the Apache implementation...)
class GammaSpec extends CustomSpec{

  "Gamma constructor" should "throw an RuntimeException if the variable's distribution type is not Gamma" in {

    Given("a multinomial variable")
    val multinomialVar = ModelVariablesFactory.newMultinomialLV("gamma wars", 2)

    When("creating a Gamma distribution from it")

    Then("a RuntimeException should be thrown")
    a[RuntimeException] should be thrownBy {
      Gamma(multinomialVar, 2.54, 0.64)
    }
  }

  it should "throw an IllegalArgumentException if the shape <= 0" in {

    Given("a gamma variable")
    val gammaVar = ModelVariablesFactory.newGammaLV("gamma_noBounds")

    When("creating a Gamma distribution with shape = -0.0001 from it")

    Then("a RuntimeException should be thrown")
    a[RuntimeException] should be thrownBy {
      Gamma(gammaVar, -0.0001, 5)
    }
  }

  it should "throw an IllegalArgumentException if the scale <= 0" in {

    Given("a gamma variable")
    val gammaVar = ModelVariablesFactory.newGammaLV("gamma_noBounds")

    When("creating a Gamma distribution with scale = -0.0001 from it")

    Then("a RuntimeException should be thrown")
    a[RuntimeException] should be thrownBy {
      Gamma(gammaVar, 5, -0.0001)
    }
  }

  "Gamma.createUsingScaleParameter" should "create a Gamma distribution using the first parametrization" in {

    Given("a gamma variable")
    val gammaVar = ModelVariablesFactory.newGammaLV("gamma_noBounds")

    When("creating a Gamma distribution using the first parametrization")
    val shape = 1.78
    val scale = 3.94
    val gammaDist = Gamma.createUsingScaleParameter(gammaVar, shape, scale)

    Then("its shape and scale parameters should coincide")
    assert(Utils.eqDouble(gammaDist.scale, scale))
    assert(Utils.eqDouble(gammaDist.shape, shape))
  }

  "Gamma.createUsingRateParameter" should "create a Gamma distribution using the second parametrization" in {

    Given("a gamma variable")
    val gammaVar = ModelVariablesFactory.newGammaLV("gamma_noBounds")

    When("creating a Gamma distribution using the second parametrization")
    val shape = 2.78
    val rate = 3.94
    val gammaDist = Gamma.createUsingRateParameter(gammaVar, shape, rate)

    Then("its shape and rate parameters should coincide")
    assert(Utils.eqDouble(gammaDist.rate, rate))
    assert(Utils.eqDouble(gammaDist.shape, shape))
  }

  "Gamma.createUsingMeanParameter" should "create a Gamma distribution using the third parametrization" in {

    Given("a gamma variable")
    val gammaVar = ModelVariablesFactory.newGammaLV("gamma_noBounds")

    When("creating a Gamma distribution using the third parametrization")
    val shape = 1.59
    val mean = 3.25
    val gammaDist = Gamma.createUsingMeanParameter(gammaVar, shape, mean)

    Then("its shape and mean parameters should coincide")
    assert(Utils.eqDouble(gammaDist.mean, mean))
    assert(Utils.eqDouble(gammaDist.shape, shape))
  }

  "Gamma.label" should "return 'Gamma'" in {
    assert(Gamma(ModelVariablesFactory.newGammaLV("gamma_noBounds"),1 , 1).label == "Gamma")
  }

  "Gamma.numberOfParameters" should "be == 2" in {
    assert(Gamma(ModelVariablesFactory.newGammaLV("gamma_noBounds"), 1 , 1).numberOfParameters == 2)
  }

  "Gamma.parameters" should "be a Vector containing the shape and scale values" in {

    Given("a gaussian variable")
    val gammaVar = ModelVariablesFactory.newGammaLV("gamma_nobounds")

    When("creating a Gamma distribution with shape = 1 and scale = 1")
    val dist = Gamma(gammaVar, 1, 1)

    Then("its parameters must be a Vector containing its mean and variance values")
    assert(dist.parameters == Vector(1, 1))
  }
}
