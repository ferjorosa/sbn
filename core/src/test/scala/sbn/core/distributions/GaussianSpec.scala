package sbn.core.distributions

import org.apache.commons.math3.util.FastMath
import sbn.core.CustomSpec
import sbn.core.utils.Utils
import sbn.core.variables.VariableFactory

class GaussianSpec extends CustomSpec{

  "Gaussian constructor" should "throw an IllegalArgumentException if the variable's distribution type is not Gaussian" in {

    Given("a multinomial variable")
    val multinomialVar = VariableFactory.newMultinomialVariable("star wars", 2)

    When("creating a Gaussian distribution from it")

    Then("a IllegalArgumentException should be thrown")
    a[IllegalArgumentException] should be thrownBy {
      Gaussian(multinomialVar, 2.54, 0.64)
    }
  }

  it should "throw an IllegalArgumentException if the variance <= 0" in {

    Given("a gaussian variable")
    val gaussianVar = VariableFactory.newGaussianVariable("gaussian_noBounds")

    When("creating a Gaussian ditribution with variance = -0.0001 from it")

    Then("a IllegalArgumentException should be thrown")
    a[IllegalArgumentException] should be thrownBy {
      Gaussian(gaussianVar, 5, -0.0001)
    }
  }

  "Gaussian.apply" should "create a standard Gaussian distribution with mean = 0 and variance = 1 when no more parameteres are passed" in {

    Given("a gaussian variable")
    val gaussianVar = VariableFactory.newGaussianVariable("gaussian", -2, 2)

    When("creating a Gaussian distribution using the apply() method")
    val dist = Gaussian(gaussianVar)

    Then("its mean must be equal to 0 and its variance must be equal to 1.0")
    assert(Utils.eqDouble(dist.mean, 0))
    assert(Utils.eqDouble(dist.variance, 1.0))
  }

  "Gaussian.label" should "return 'Gaussian'" in {
    assert(Gaussian(VariableFactory.newGaussianVariable("gaussian_noBounds")).label == "Gaussian")
  }

  "Gaussian.numberOfParameters" should "be == 2" in {
    assert(Gaussian(VariableFactory.newGaussianVariable("gaussian_noBounds")).numberOfParameters == 2)
  }

  "Gaussian.parameters" should "be a Vector containing the mean and variance values" in {

    Given("a gaussian variable")
    val gaussianVar = VariableFactory.newGaussianVariable("gaussian_noBounds")

    When("creating a standard Gaussian distribution using the apply() method")
    val dist = Gaussian(gaussianVar)

    Then("its parameters must be a Vector containing its mean and variance values")
    assert(dist.parameters == Vector(0, 1))
  }

  "Gaussian.probability(x)" should "return P(X = x)" in {

    Given("a gaussian variable")
    val gaussianVar = VariableFactory.newGaussianVariable("gaussian_noBounds")

    When("creating a standard Gaussian distribution using the apply() method")
    val dist = Gaussian(gaussianVar)

    Then("for any point x, probability(x) must return 0")
    assert(Utils.eqDouble(dist.probability(7.45), 0))
    assert(Utils.eqDouble(dist.probability(0.0), 0))
    assert(Utils.eqDouble(dist.probability(-1000.57), 0))
  }

  "Gaussian.logProbability(x)" should "return log P(X = x)" in {

    Given("a gaussian variable")
    val gaussianVar = VariableFactory.newGaussianVariable("gaussian_noBounds")

    When("creating a standard Gaussian distribution using the apply() method")
    val dist = Gaussian(gaussianVar)

    Then("for any point x, log probability(x) must return -infinity")
    assert(dist.logProbability(7.45) == Double.NegativeInfinity)
    assert(dist.logProbability(0.0) == Double.NegativeInfinity)
    assert(dist.logProbability(-1000.57) == Double.NegativeInfinity)
  }

  "Gaussian.probability(x0, x1)" should "return P(x0 < X <= x1)" in {

    Given("a gaussian variable")
    val gaussianVar = VariableFactory.newGaussianVariable("gaussian_noBounds")

    When("creating a standard Gaussian distribution using the apply() method")
    val dist = Gaussian(gaussianVar)

    Then("probability(-0.33, 0.33) must equal cumulativeProbability(-0.33) - cumulativeProbability(0.33)")
    assert(Utils.eqDouble(dist.probability(-0.33, 0.33), dist.cumulativeProbability(0.33) - dist.cumulativeProbability(-0.33)))

    And("probability(0.5, 5.6) must equal cumulativeProbability(0.5) - cumulativeProbability(5.6)")
    assert(Utils.eqDouble(dist.probability(0.5, 5.6), dist.cumulativeProbability(5.6) - dist.cumulativeProbability(0.5)))
  }

  "Gaussian.cumulativeProbability(x)" should "return P(X <= x)" in {

    Given("a gaussian variable")
    val gaussianVar = VariableFactory.newGaussianVariable("gaussian_noBounds")

    When("creating a standard Gaussian distribution using the apply() method")
    val dist = Gaussian(gaussianVar)

    Then("cumulativeProbability(0.5) must equal 0.69146 with an epsilon of 0.00001")
    assert(Utils.eqDouble(dist.cumulativeProbability(0.5), 0.69146, 0.00001))

    And("cumulativeProbability(-0.3) must equal 0.38208 with an epsilon of 0.00001")
    assert(Utils.eqDouble(dist.cumulativeProbability(-0.3), 0.38208, 0.00001))

    And("cumulativeProbability(0.0) must equal 0.5 with an epsilon of 0.00001")
    assert(Utils.eqDouble(dist.cumulativeProbability(0.0), 0.5, 0.00001))
  }

  "Gaussian.density(x)" should "return pdf(X = x)" in {

    Given("a gaussian variable")
    val gaussianVar = VariableFactory.newGaussianVariable("gaussian_noBounds")

    When("creating a standard Gaussian distribution using the apply() method")
    val dist = Gaussian(gaussianVar)

    Then("density(-0.5) must equal 0.35206 with an epsilon of 0.00001")
    assert(Utils.eqDouble(dist.density(-0.5), 0.35206, 0.00001))

    And("density(1) must equal 0.24197 with an epsilon of 0.00001")
    assert(Utils.eqDouble(dist.density(1), 0.24197, 0.00001))

    And("density(0) must equal 0.39894 with an epsilon of 0.00001")
    assert(Utils.eqDouble(dist.density(0), 0.39894, 0.00001))
  }

  "Gaussian.logDensity(x)" should "return log pdf(X = x)" in {

    Given("a gaussian variable")
    val gaussianVar = VariableFactory.newGaussianVariable("gaussian_noBounds")

    When("creating a standard Gaussian distribution using the apply() method")
    val dist = Gaussian(gaussianVar)

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
