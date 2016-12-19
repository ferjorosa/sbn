package sbn.core.statistics.distributions.exponentialfamily

import org.apache.commons.math3.util.FastMath
import sbn.core.CustomSpec
import sbn.core.statistics.distributions.Multinomial
import sbn.core.utils.Utils
import sbn.core.variables.model.ModelVariablesFactory

/**
  * Created by fer on 13/12/16.
  */
class EF_MultinomialSpec extends CustomSpec{

  "EF_Multinomial constructor" should "throw an IllegalArgumentException if variable.distributionType is not MultinomialType" in {

    Given("a variable of Gaussian type")
    val variable = ModelVariablesFactory.newGaussianLV("multinomial")

    When("Creating a EF_Multinomial distribution from it")

    Then("an IllegalArgumentException should be thrown")
    a[IllegalArgumentException] should be thrownBy {
      EF_Multinomial(variable, Vector(0.0, 0.5, 0.5))
    }
  }

  it should "throw an IllegalArgumentException if the numberOfStates of the variable =! probabilities.size" in {
    a[IllegalArgumentException] should be thrownBy {
      EF_Multinomial(ModelVariablesFactory.newMultinomialLV("multinomial", 2), Vector(0.33, 0.33, 0.34))
    }
  }

  it should "throw an IllegalArgumentException if the sum of probabilities != 1.0" in {
    a[IllegalArgumentException] should be thrownBy {
      EF_Multinomial(ModelVariablesFactory.newMultinomialLV("multinomial", 2), Vector(0.5, 0.0))
    }
  }

  "EF_Multinomial.apply" should "create a Multinomial distribution with random probabilities from a finite state variable" in {

    Given("a multinomial variable with 3 states")
    val variable = ModelVariablesFactory.newMultinomialLV("multinomial", 3)

    When("creating an EF_Multinomial distribution from it")
    val distribution = EF_Multinomial(variable)

    Then("the number of parameters must equal the number of states of the variable")
    assert(distribution.probabilities.size == 3)

    And("The probabilities must sum 1.0")
    assert(Utils.eqDouble(distribution.probabilities.sum, 1.0))
  }



  "Multinomial.density" should "return P(X = x)" in {

    Given("a multinomial variable with 3 states")
    val variable = ModelVariablesFactory.newMultinomialLV("multinomial", 3)

    When("Creating an EF_Multinomial distribution with parameters (0.2, 0.5, 0.3) from it ")
    val dist = EF_Multinomial(variable, Vector(0.2, 0.5, 0.3))

    Then("density(1) must equal 0.5 and probability(1)")
    assert(Utils.eqDouble(dist.density(1), 0.5))
    assert(Utils.eqDouble(dist.density(1), dist.probabilities(1)))

    And("density(0) must equal 0.2 and probability(0)")
    assert(Utils.eqDouble(dist.density(0), 0.2))
    assert(Utils.eqDouble(dist.density(0), dist.probabilities(0)))
  }

  "EF_Multinomial.logDensity" should "return log P(X = x)" in {

    Given("a multinomial variable with 3 states")
    val variable = ModelVariablesFactory.newMultinomialLV("multinomial", 3)

    When("Creating a multinomial distribution with parameters (0.2, 0.5, 0.3) from it ")
    val dist = EF_Multinomial(variable, Vector(0.2, 0.5, 0.3))

    Then("log(density(0)) must equal logDensity(0)")
    assert(Utils.eqDouble(dist.density(0), 0.2))
    assert(Utils.eqDouble(Math.log(dist.density(0)), dist.logDensity(0)))

    And("logDensity(0) must equal logProbability(0)")
    assert(Utils.eqDouble(dist.logDensity(0), FastMath.log(dist.probabilities(0))))

    And("exp(logDensity(1)) must equal density(1) and probability(1)")
    assert(Utils.eqDouble(Math.exp(dist.logDensity(1)), dist.density(1)))
  }
}
