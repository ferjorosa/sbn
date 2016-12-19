package sbn.core.statistics.distributions

import sbn.core.CustomSpec
import sbn.core.data.attributes.{Attribute, FiniteStateSpace}
import sbn.core.utils.Utils
import sbn.core.variables.model.ModelVariablesFactory

class MultinomialSpec extends CustomSpec{

  "Multinomial constructor" should "throw a RuntimeException if variable.distributionType is not MultinomialType" in {

    Given("a variable of Gaussian type")
    val variable = ModelVariablesFactory.newGaussianLV("tonto el que lo lea")

    When("Creating a Multinomial distribution from it")

    Then("a RuntimeException should be thrown")
    a[RuntimeException] should be thrownBy {
      Multinomial(variable, Vector(0.0, 0.5, 0.5))
    }
  }

  it should "throw a RuntimeException if the numberOfStates of the variable =! probabilities.size" in {
    a[RuntimeException] should be thrownBy {
      Multinomial(ModelVariablesFactory.newMultinomialLV("multinomial", 2), Vector(0.33, 0.33, 0.34))
    }
  }

  it should "throw a RuntimeException if the sum of probabilities != 1.0" in {
    a[RuntimeException] should be thrownBy {
      Multinomial(ModelVariablesFactory.newMultinomialLV("multinomial", 2), Vector(0.5, 0))
    }
  }

  "Multinomial.apply" should "create a Multinomial distribution with random probabilities from a finite state variable" in {

    Given("a multinomial variable with 3 states")
    val variable = ModelVariablesFactory.newMultinomialLV("multinomial", 3)

    When("creating a multinomial distribution from it")
    val distribution = Multinomial(variable)

    Then("the number of parameters must equal the number of states of the variable")
    assert(distribution.numberOfParameters == 3)

    And("The probabilities must sum 1.0")
    assert(Utils.eqDouble(distribution.probabilities.sum, 1.0))
  }

  "Multinomial.label" should "return 'Multinomial'" in {
    assert(Multinomial(ModelVariablesFactory.newMultinomialLV("multinomial", 2)).label == "Multinomial")
  }

  "Multinomial.numberOfParameters" should "be equal to the nStates of the variable and its probabilities.size" in {

    Given("a multinomial variable with 3 states")
    val variable = ModelVariablesFactory.newMultinomialLV("multinomial", 3)

    When("creating a multinomial distribution from it")
    val dist = Multinomial(variable)

    Then("the number of parameters of the distribution should equal the number of states of the variable")
    val finiteStateSpace = variable.attribute.stateSpaceType match{
      case finite: FiniteStateSpace => finite
      case _ => throw new IllegalStateException("state space should be finite")
    }
    assert(finiteStateSpace.numberOfStates ==  dist.numberOfParameters)
  }

  "Multinomial.probability(x)" should "return P(X = x)" in {

    Given("a multinomial variable with 3 states")
    val variable = ModelVariablesFactory.newMultinomialLV("multinomial", 3)

    When("Creating a multinomial distribution with parameters (0.2, 0.5, 0.3) from it ")
    val dist = Multinomial(variable, Vector(0.2, 0.5, 0.3))

    Then("probability(1) must equal 0.5")
    assert(Utils.eqDouble(dist.probability(1), 0.5))

    And("probability(0) must equal 0.2")
    assert(Utils.eqDouble(dist.probability(0), 0.2))
  }

  "Multinomial.logProbability(x)" should "return log P(X = x)" in {

    Given("a multinomial variable with 3 states")
    val variable = ModelVariablesFactory.newMultinomialLV("multinomial", 3)

    When("Creating a multinomial distribution with parameters (0.2, 0.5, 0.3) from it ")
    val dist = Multinomial(variable, Vector(0.2, 0.5, 0.3))

    Then("log(probability(0)) must equal logProbability(0)")
    assert(Utils.eqDouble(dist.probability(0), 0.2))
    assert(Utils.eqDouble(Math.log(dist.probability(0)), dist.logProbability(0)))

    And("exp(logProbability(1)) must equal probability(1)")
    assert(Utils.eqDouble(Math.exp(dist.logProbability(1)), dist.probability(1)))
  }

  "Multinomial.probability(x0, x1)" should "return P(x0 < X <= x1)" in {

    Given("a multinomial variable with 6 states")
    val variable = ModelVariablesFactory.newMultinomialLV("multinomial", 6)

    When("creating a multinomial distribution with parameters (0.2, 0.1, 0.3, 0.2, 0.05, 0.15) from it")
    val dist = Multinomial(variable, Vector(0.2, 0.1, 0.3, 0.2, 0.05, 0.15))

    Then("probability(0, 5) must equal 0.8")
    assert(Utils.eqDouble(dist.probability(0, dist.parameters.size - 1), 0.8))

    And("probability(2, 4) must equal 0.")
    assert(Utils.eqDouble(dist.probability(2, 4), 0.25))

  }

  "Multinomial.cumulativeProbability(x)" should "return P(X <= x)" in {

    Given("a multinomial variable with 6 states")
    val variable = ModelVariablesFactory.newMultinomialLV("multinomial", 6)

    When("creating a multinomial distribution with parameters (0.2, 0.1, 0.3, 0.2, 0.05, 0.15) from it")
    val dist = Multinomial(variable, Vector(0.2, 0.1, 0.3, 0.2, 0.05, 0.15))

    Then("cumulativeProbability(5) must equal 1.0")
    assert(Utils.eqDouble(dist.cumulativeProbability(dist.parameters.size - 1), 1.0))

    And("cumulativeProbability(0) must equal 0.2")
    assert(Utils.eqDouble(dist.cumulativeProbability(0), 0.2))

    And("cumulativeProbability(4) must equal 0.85")
    assert(Utils.eqDouble(dist.cumulativeProbability(4), 0.85))
  }

  "Multinomial.density" should "return P(X = x)" in {

    Given("a multinomial variable with 3 states")
    val variable = ModelVariablesFactory.newMultinomialLV("multinomial", 3)

    When("Creating a multinomial distribution with parameters (0.2, 0.5, 0.3) from it ")
    val dist = Multinomial(variable, Vector(0.2, 0.5, 0.3))

    Then("density(1) must equal 0.5 and probability(1)")
    assert(Utils.eqDouble(dist.density(1), 0.5))
    assert(Utils.eqDouble(dist.density(1), dist.probability(1)))

    And("density(0) must equal 0.2 and probability(0)")
    assert(Utils.eqDouble(dist.density(0), 0.2))
    assert(Utils.eqDouble(dist.density(0), dist.probability(0)))
  }

  "Multinomial.logDensity" should "return log P(X = x)" in {

    Given("a multinomial variable with 3 states")
    val variable = ModelVariablesFactory.newMultinomialLV("multinomial", 3)

    When("Creating a multinomial distribution with parameters (0.2, 0.5, 0.3) from it ")
    val dist = Multinomial(variable, Vector(0.2, 0.5, 0.3))

    Then("log(density(0)) must equal logDensity(0)")
    assert(Utils.eqDouble(dist.density(0), 0.2))
    assert(Utils.eqDouble(Math.log(dist.density(0)), dist.logDensity(0)))

    And("logDensity(0) must equal logProbability(0)")
    assert(Utils.eqDouble(dist.logDensity(0), dist.logProbability(0)))

    And("exp(logDensity(1)) must equal density(1) and probability(1)")
    assert(Utils.eqDouble(Math.exp(dist.logDensity(1)), dist.density(1)))
  }

  "Multinomial.sample" should "return a valid value" in {

    Given("a distribution with 4 random parameters")
    val dist = Multinomial(ModelVariablesFactory.newMultinomialLV("multinomial", 4))

    When("values are sampled")
    val sampledValues: Seq[Double] = for(i<-0 until 100) yield dist.sample

    Then("No sampled value can be an state index that is out of bounds")
    assert(sampledValues.filter(_ >= 4).count(_ < 0) == 0)
  }

  "Multinomial.getStateName" should "return valid state names when parameter names have been defined by default" in {

    Given("a distribution with parameter names by default")
    val dist = Multinomial(ModelVariablesFactory.newMultinomialLV("multinomial", 4))

    When("getting the parameter names")
    val stateNames = for (i <- dist.parameters.indices) yield dist.getStateName(i)

    Then("it should return 's'+ index of the parameter")
    assert(stateNames.toList equals List("s0", "s1", "s2", "s3"))
  }

  it should "return valid state names when parameter names have been defined by the variable's attribute" in {

    Given("a distribution with parameter names defined by the variable's attribute")
    val attribute = Attribute("manifestAttr1", FiniteStateSpace(Vector("estado1", "s2", "attrState3")))
    val dist2 = Multinomial(ModelVariablesFactory.newMultinomialMV(attribute))

    When("getting the parameter names")
    val stateNames2 = for(i <- dist2.parameters.indices)yield dist2.getStateName(i)

    Then("it should return the names provided by the variable's attribute")
    assert(stateNames2.toList equals List("estado1", "s2", "attrState3"))
  }

}
