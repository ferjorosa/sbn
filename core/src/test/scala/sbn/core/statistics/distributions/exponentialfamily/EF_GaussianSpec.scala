package sbn.core.statistics.distributions.exponentialfamily

import breeze.linalg.DenseVector
import org.apache.commons.math3.util.FastMath
import sbn.core.CustomSpec
import sbn.core.utils.Utils
import sbn.core.variables.model.ModelVariablesFactory

/**
  * Created by fer on 13/12/16.
  */
class EF_GaussianSpec extends CustomSpec{

  "EF_Gaussian constructor" should "throw a RuntimeException if the variable's distribution type is not Gaussian" in {

    Given("a multinomial variable")
    val multinomialVar = ModelVariablesFactory.newMultinomialLV("star wars rogue", 2)

    When("creating a Gaussian distribution from it")

    Then("a RuntimeException should be thrown")
    a[RuntimeException] should be thrownBy {
      EF_Gaussian(multinomialVar, 2.54, 0.64)
    }
  }

  it should "throw a RuntimeException if the variance <= 0" in {

    Given("a gaussian variable")
    val gaussianVar = ModelVariablesFactory.newGaussianLV("gaussian_noBounds")

    When("creating a Gaussian ditribution with variance = -0.0001 from it")

    Then("a RuntimeException should be thrown")
    a[RuntimeException] should be thrownBy {
      EF_Gaussian(gaussianVar, 5, -0.0001)
    }
  }

  "Gaussian.apply" should "create a standard Gaussian distribution with mean = 0 and variance = 1 when no more parameteres are passed" in {

    Given("a gaussian variable")
    val gaussianVar = ModelVariablesFactory.newGaussianLV("gaussian", -2, 2)

    When("creating a Gaussian distribution using the apply() method")
    val dist = EF_Gaussian(gaussianVar)

    Then("its mean must be equal to 0 and its variance must be equal to 1.0")
    assert(Utils.eqDouble(dist.mean, 0))
    assert(Utils.eqDouble(dist.variance, 1.0))
  }

  "EF_Gaussian.momentParameters" should "be a Vector containing the mean and variance values" in {

    Given("a gaussian variable")
    val gaussianVar = ModelVariablesFactory.newGaussianLV("gaussian")

    When("creating a Gaussian distribution with mean = 0.5 and variance = 3.84")
    val dist = EF_Gaussian(gaussianVar, 0.5, 3.84)

    Then("its moment parameters must be a Vector containing its mean and variance values")
    assert(dist.momentParameters == DenseVector(0.5, 3.84))
  }

  "EF_Gaussian.naturalParameters" should "be a Vector of (mean / variance, - 1 / (2 * variance))" in {

    Given("a gaussian variable")
    val gaussianVar = ModelVariablesFactory.newGaussianLV("gaussian")

    When("creating a Gaussian distribution with shape = 0.5 and scale = 1.76")
    val dist = EF_Gaussian(gaussianVar, 0.5, 1.76)

    Then("its natural parameters must be a Vector of (mean / variance, - 1 / (2 * variance))")
    assert(dist.naturalParameters == DenseVector(0.5 / 1.76, - 1 / (2 * 1.76)))
  }

  "Gaussian.density(x)" should "return pdf(X = x)" in {

    Given("a gaussian variable")
    val gaussianVar = ModelVariablesFactory.newGaussianLV("gaussian_noBounds")

    When("creating a standard Gaussian distribution using the apply() method")
    val dist = EF_Gaussian(gaussianVar)

    Then("density(-0.5) must equal 0.35206 with an epsilon of 0.00001")
    assert(Utils.eqDouble(dist.density(-0.5), 0.35206, 0.00001))

    And("density(1) must equal 0.24197 with an epsilon of 0.00001")
    assert(Utils.eqDouble(dist.density(1), 0.24197, 0.00001))

    And("density(0) must equal 0.39894 with an epsilon of 0.00001")
    assert(Utils.eqDouble(dist.density(0), 0.39894, 0.00001))
  }

  "Gaussian.logDensity(x)" should "return log pdf(X = x)" in {

    Given("a gaussian variable")
    val gaussianVar = ModelVariablesFactory.newGaussianLV("gaussian_noBounds")

    When("creating a standard Gaussian distribution using the apply() method")
    val dist = EF_Gaussian(gaussianVar)

    Then("logDensity(-0.5) must equal log(0.35206) with an epsilon of 0.0001")
    // one 0 less because the error adds up
    assert(Utils.eqDouble(dist.logDensity(-0.5), FastMath.log(0.35206), 0.0001))

    And("logDensity(1) must equal log(0.24197) with an epsilon of 0.0001")
    // one 0 less because the error adds up
    assert(Utils.eqDouble(dist.logDensity(1), FastMath.log(0.24197), 0.0001))

    And("logDensity(0) must equal log(0.39894) with an epsilon of 0.0001")
    // one 0 less because the error adds up
    assert(Utils.eqDouble(dist.logDensity(0), FastMath.log(0.39894), 0.0001))
  }

}
