package sbn.core.statistics.distributions

import org.apache.commons.math3.util.FastMath
import sbn.core.CustomSpec
import sbn.core.utils.Utils
import sbn.core.variables.model.ModelVariablesFactory
import sbn.core.variables.{Assignment, Assignments}

class Multinomial_MultinomialParentsSpec extends CustomSpec{

  "Multinomial_MultinomialParents.apply" should "throw a RuntimeException if the variable is not of MultinomialType" in{

    Given("a variable of Gaussian type and a set of multinomial parents")
    val variable = ModelVariablesFactory.newGaussianLV("gaussian")
    val parent1 = ModelVariablesFactory.newMultinomialLV("mult1", 2)
    val parent2 = ModelVariablesFactory.newMultinomialLV("mult2", 3)

    When("creating a Multinomial_MultinomialParents distribution from it")

    Then("a RuntimeException should be thrown")
    a[RuntimeException] should be thrownBy {
      Multinomial_Multinomial(variable, Vector(parent1, parent2))
    }
  }

  it should "throw a RuntimeException if parents are not exclusively of MultinomialType" in {

    Given("a variable of Multinomial type and a set of mixed-type parents")
    val variable = ModelVariablesFactory.newMultinomialLV("multinomial",3)
    val parent1 = ModelVariablesFactory.newGaussianLV("gaussianP")
    val parent2 = ModelVariablesFactory.newMultinomialLV("multinomialP", 3)

    When("creating a Multinomial_MultinomialParents distribution from it")

    Then("a RuntimeException should be thrown")
    a[RuntimeException] should be thrownBy {
      Multinomial_Multinomial(variable, Vector(parent1, parent2))
    }
  }

  "Multinomial_MultinomialParents.label" should "return 'Multinomial | Multinomial'" in {

   Given("a variable of multinomial type and a set of multinomial parents")
    val variable = ModelVariablesFactory.newMultinomialLV("multinomial",3)
    val parent1 = ModelVariablesFactory.newMultinomialLV("mult1", 1)
    val parent2 = ModelVariablesFactory.newMultinomialLV("mult2", 4)

    When("creating a Multinomial_MultinomialParents distribution from it")
    val dist = Multinomial_Multinomial(variable, Vector(parent1, parent2))

    Then("its label must be 'Multinomial | Multinomial'")
    assert(dist.label == "Multinomial | Multinomial")
  }

  "Multinomial_MultinomialParents.numberOfParameters" should "return the valid number of parameters of the distribution" in{

    Given("a multinomial variable with 4 parameters and a set of 2 multinomial parents with 3 and 8 parameters respectively")
    val variable = ModelVariablesFactory.newMultinomialLV("multinomial",3)
    val parent1 = ModelVariablesFactory.newMultinomialLV("mult1", 4)
    val parent2 = ModelVariablesFactory.newMultinomialLV("mult2", 8)

    When("creating a Multinomial_MultinomialParents distribution from it")
    val dist = Multinomial_Multinomial(variable, Vector(parent1, parent2))

    Then("this distribution should have 3*(4*8) parameters")
    assert(dist.numberOfParameters == 3*4*8)
  }

  /** These variable are all immutable objects with immutable references so they are safe to use in multiple tests */
  val mult_3states = ModelVariablesFactory.newMultinomialLV("multinomial",3)
  val parent1_2states = ModelVariablesFactory.newMultinomialLV("mult1", 2)
  val parent2_2states = ModelVariablesFactory.newMultinomialLV("mult2", 2)
  // Manually created distributions
  val mult1_0_mult2_0 = Multinomial(mult_3states,Vector(0.9, 0.1, 0.0))
  val mult1_0_mult2_1 = Multinomial(mult_3states,Vector(0.5, 0.4, 0.1))
  val mult1_1_mult2_0 = Multinomial(mult_3states,Vector(0.3, 0.2, 0.5))
  val mult1_1_mult2_1 = Multinomial(mult_3states,Vector(0.01, 0.99, 0.0))
  //Manually created assignments (they are usually created automatically with Utils.cartesianProduct)
  val assignments0_0 = Assignments(Set(Assignment(parent1_2states,0), Assignment(parent2_2states,0)))
  val assignments0_1 = Assignments(Set(Assignment(parent1_2states,0), Assignment(parent2_2states,1)))
  val assignments1_0 = Assignments(Set(Assignment(parent1_2states,1), Assignment(parent2_2states,0)))
  val assignments1_1 = Assignments(Set(Assignment(parent1_2states,1), Assignment(parent2_2states,1)))
  // Manually assign each distribution to its parent assignment
  val parameterizedDistributions = Map(assignments0_0 -> mult1_0_mult2_0,
                                       assignments0_1 -> mult1_0_mult2_1,
                                       assignments1_0 -> mult1_1_mult2_0,
                                       assignments1_1 -> mult1_1_mult2_1)

  "Multinomial_MultinomialParents.getUnivariateDistribution" should "return the correct Multinomial distribution" in{

    Given("a multinomial variable with 3 parameters and a set of 2 multinomial parents with 2 parameters each (manual parameters)")

    When("creating a Multinomial_MultinomialParents distribution from it")
    val dist = Multinomial_Multinomial(mult_3states, Vector(parent1_2states, parent2_2states), parameterizedDistributions)

    Then("getUnivariateDistribution(parent1_2states = 0, parent2_2states = 1) should return the multinomial distribution with parameters [0.5, 0.4, 0.1]")
    assert(dist.getUnivariateDistribution(assignments0_1).parameters equals Vector(0.5, 0.4, 0.1))

    And("getUnivariateDistribution(parent1_2states = 1, parent2_2states = 1) should return the multinomial distribution with parameters [0.01, 0.99, 0.0]")
    assert(dist.getUnivariateDistribution(assignments1_1).parameters equals Vector(0.01, 0.99, 0.0))

    And("getUnivariateDistribution(parent1_2states = -1, parent2_2states = 1) should throw a RuntimeException")
    a[RuntimeException] should be thrownBy {
      dist.getUnivariateDistribution(Assignments(Set(Assignment(parent1_2states, -1), Assignment(parent2_2states, 1))))
    }

    And("getUnivariateDistribution(newVariable = 0, parent2_2states = 1) should throw a RuntimeException")
    a[RuntimeException] should be thrownBy {
      val newVariable = ModelVariablesFactory.newMultinomialLV("newVariable", 2)
      dist.getUnivariateDistribution(Assignments(Set(Assignment(newVariable, 0), Assignment(parent2_2states, 1))))
    }

    And("getUnivariateDistribution(parent2_2states = 1) should throw a RuntimeException")
    a[RuntimeException] should be thrownBy {
      dist.getUnivariateDistribution(Assignments(Set(Assignment(parent2_2states,0))))
    }
  }

  "Multinomial_MultinomialParents.conditionalProbability(assignments, x)" should "return  P(X = x | assignments)" in{

    Given("a multinomial variable with 3 parameters and a set of 2 multinomial parents with 2 parameters each (manual parameters)")

    When("creating a Multinomial_MultinomialParents distribution from it")
    val dist = Multinomial_Multinomial(mult_3states, Vector(parent1_2states, parent2_2states), parameterizedDistributions)

    Then("conditionalProbability(X = 1 | parent1_2states = 0, parent2_2states = 1) should return 0.4")
    assert(Utils.eqDouble(dist.conditionalProbability(assignments0_1, 1), 0.4))

    And("conditionalProbability(X = 2 | parent1_2states = 1, parent2_2states = 1) should return 0.0")
    assert(Utils.eqDouble(dist.conditionalProbability(assignments1_1, 2), 0.0))

    And("conditionalProbability(X = 6 | parent1_2states = 1, parent2_2states = 1) should throw a RuntimeException (X = 6 is an invalid value)")
    a[RuntimeException] should be thrownBy {
      dist.conditionalProbability(assignments1_1, 6)
    }
  }

  "Multinomial_MultinomialParents.logConditionalProbability(assignments, x)" should "return  log P(X = x | assignments)" in{

    Given("a multinomial variable with 3 parameters and a set of 2 multinomial parents with 2 parameters each (manual parameters)")

    When("creating a Multinomial_MultinomialParents distribution from it")
    val dist = Multinomial_Multinomial(mult_3states, Vector(parent1_2states, parent2_2states), parameterizedDistributions)

    Then("logConditionalProbability(X = 1 | parent1_2states = 0, parent2_2states = 1) should return log(0.4)")
    assert(Utils.eqDouble(dist.logConditionalProbability(assignments0_1, 1), FastMath.log(0.4)))

    And("logConditionalProbability(X = 2 | parent1_2states = 1, parent2_2states = 1) should return log(0.0)")
    // -infinity
    assert(dist.logConditionalProbability(assignments1_1, 2) == FastMath.log(0.0))

    And("logConditionalProbability(X = 6 | parent1_2states = 1, parent2_2states = 1) should throw a RuntimeException (X = 6 is an invalid value)")
    a[RuntimeException] should be thrownBy {
      dist.logConditionalProbability(assignments1_1, 6)
    }
  }

  "Multinomial_MultinomialParents.conditionalProbability(assignments, x0, x1)" should "return P(x0 < X <= x1 | assignments)" in{

    Given("a multinomial variable with 3 parameters and a set of 2 multinomial parents with 2 parameters each (manual parameters)")

    When("creating a Multinomial_MultinomialParents distribution from it")
    val dist = Multinomial_Multinomial(mult_3states, Vector(parent1_2states, parent2_2states), parameterizedDistributions)

    Then("conditionalProbability(1 < X <= 2 | parent1_2states = 0, parent2_2states = 1) should return 0.1")
    assert(Utils.eqDouble(dist.conditionalProbability(assignments0_1, 1, 2), 0.1))

    And("conditionalProbability(-1 < X <= 2 | parent1_2states = 1, parent2_2states = 0) should throw a RuntimeException (x0 = -1 is an invalid value)")
    a[RuntimeException] should be thrownBy {
      println(dist.conditionalProbability(assignments1_0, -1, 2))
    }

    And("conditionalProbability(1 < X <= 0 | parent1_2states = 1, parent2_2states = 1) should throw aRuntimeException (x0 > x1)")
    a[RuntimeException] should be thrownBy {
      dist.conditionalProbability(assignments1_1, 1, 0)
    }
  }

  "Multinomial_MultinomialParents.cumulativeConditionalProbability(assignments, x)" should "return  P(X <= x | assignments)" in{

    Given("a multinomial variable with 3 parameters and a set of 2 multinomial parents with 2 parameters each (manual parameters)")

    When("creating a Multinomial_MultinomialParents distribution from it")
    val dist = Multinomial_Multinomial(mult_3states, Vector(parent1_2states, parent2_2states), parameterizedDistributions)

    Then("cumulativeConditionalProbability(X <= 2 | parent1_2states = 1, parent2_2states = 1) should return 1.0")
    assert(Utils.eqDouble(dist.cumulativeConditionalProbability(assignments0_1, 2), 1.0))

    And("cumulativeConditionalProbability(X <= 7 | parent1_2states = 0, parent2_2states = 1) should throw a RuntimeException")
    a[RuntimeException] should be thrownBy {
      dist.logConditionalProbability(assignments0_1, 7)
    }
    And("cumulativeConditionalProbability(X <= -1 | parent1_2states = 1, parent2_2states = 0) should throw a RuntimeException")
    a[RuntimeException] should be thrownBy {
      dist.logConditionalProbability(assignments1_0, -1)
    }
  }

  "Multinomial_MultinomialParents.conditionalDensity(assignments, x)" should "return P(X = x | assignments)" in {

    Given("a multinomial variable with 3 parameters and a set of 2 multinomial parents with 2 parameters each (manual parameters)")

    When("creating a Multinomial_MultinomialParents distribution from it")
    val dist = Multinomial_Multinomial(mult_3states, Vector(parent1_2states, parent2_2states), parameterizedDistributions)

    Then("conditionalDensity(X = 1 | parent1_2states = 0, parent2_2states = 1) should return 0.4")
    assert(Utils.eqDouble(dist.conditionalDensity(assignments0_1, 1), 0.4))

    And("conditionalDensity(X = 2 | parent1_2states = 1, parent2_2states = 1) should return 0.0")
    assert(Utils.eqDouble(dist.conditionalDensity(assignments1_1, 2), 0.0))

    And("conditionalDensity(X = 6 | parent1_2states = 1, parent2_2states = 1) should throw a RuntimeException (X = 6 is an invalid value)")
    a[RuntimeException] should be thrownBy {
      dist.conditionalDensity(assignments1_1, 6)
    }
  }

  "Multinomial_MultinomialParents.logConditionalDensity(assignments, x)" should "return P(X = x | assignments)" in {

    Given("a multinomial variable with 3 parameters and a set of 2 multinomial parents with 2 parameters each (manual parameters)")

    When("creating a Multinomial_MultinomialParents distribution from it")
    val dist = Multinomial_Multinomial(mult_3states, Vector(parent1_2states, parent2_2states), parameterizedDistributions)

    Then("logConditionalDensity(X = 1 | parent1_2states = 0, parent2_2states = 1) should return log(0.4)")
    assert(Utils.eqDouble(dist.logConditionalDensity(assignments0_1, 1), FastMath.log(0.4)))

    And("logConditionalDensity(X = 2 | parent1_2states = 1, parent2_2states = 1) should return log(0.0)")
    // -infinity
    assert(dist.logConditionalDensity(assignments1_1, 2) == FastMath.log(0.0))

    And("logConditionalDensity(X = 6 | parent1_2states = 1, parent2_2states = 1) should throw a RuntimeException (X = 6 is an invalid value)")
    a[RuntimeException] should be thrownBy {
      dist.logConditionalDensity(assignments1_1, 6)
    }
  }

  "Multinomial_MultinomialParents.toEF_Distribution" should "return an equivalent EF_Multinomial_Multinomial object" in {

    Given("a multinomial variable with 3 parameters and a set of 2 multinomial parents with 2 parameters each (manual parameters)")

    When("creating a Multinomial_MultinomialParents distribution from it")
    val dist = Multinomial_Multinomial(mult_3states, Vector(parent1_2states, parent2_2states), parameterizedDistributions)

    Then("dist.toEF_Distribution should return an equivalent EF_Multinomial_Multinomial object")
    val ef_dist = dist.toEF_Distribution
    assert(dist.variable equals ef_dist.variable)
    assert(dist.parents equals ef_dist.parents)

    val distAssignments = dist.assignedDistributions.keys
    val ef_distAssignments = ef_dist.assignedDistributions.keys

    assert(distAssignments.size == ef_distAssignments.size)
    // Compare both distributions probabilities (their moment parameters)
    distAssignments.foreach{x =>
      assert(
        dist.assignedDistributions(x).probabilities
        equals
        ef_dist.assignedDistributions(x).probabilities
      )}
  }
}
