package sbn.core.distributions

import org.apache.commons.math3.util.FastMath
import sbn.core.CustomSpec
import sbn.core.utils.Utils
import sbn.core.variables.{Assignment, Assignments, VariableFactory}

class Multinomial_MultinomialParentsSpec extends CustomSpec{

  "Multinomial_MultinomialParents.apply" should "throw an IllegalArgumentException if the variable is not of MultinomialType" in{

    Given("a variable of Gaussian type and a set of multinomial parents")
    val variable = VariableFactory.newGaussianVariable("gaussian")
    val parent1 = VariableFactory.newMultinomialVariable("mult1", 2)
    val parent2 = VariableFactory.newMultinomialVariable("mult2", 3)

    When("creating a Multinomial_MultinomialParents distribution from it")

    Then("a IllegalArgumentException should be thrown")
    a[IllegalArgumentException] should be thrownBy {
      Multinomial_MultinomialParents(variable, Set(parent1, parent2))
    }
  }

  it should "throw an IllegalArgumentException if parents are not exclusively of MultinomialType" in {

    Given("a variable of Multinomial type and a set of mixed-type parents")
    val variable = VariableFactory.newMultinomialVariable("multinomial",3)
    val parent1 = VariableFactory.newGaussianVariable("gaussianP")
    val parent2 = VariableFactory.newMultinomialVariable("multinomialP", 3)

    When("creating a Multinomial_MultinomialParents distribution from it")

    Then("a IllegalArgumentException should be thrown")
    a[IllegalArgumentException] should be thrownBy {
      Multinomial_MultinomialParents(variable, Set(parent1, parent2))
    }
  }

  "Multinomial_MultinomialParents.label" should "return 'Multinomial | Multinomial'" in {

   Given("a variable of multinomial type and a set of multinomial parents")
    val variable = VariableFactory.newMultinomialVariable("multinomial",3)
    val parent1 = VariableFactory.newMultinomialVariable("mult1", 1)
    val parent2 = VariableFactory.newMultinomialVariable("mult2", 4)

    When("creating a Multinomial_MultinomialParents distribution from it")
    val dist = Multinomial_MultinomialParents(variable, Set(parent1, parent2))

    Then("its label must be 'Multinomial | Multinomial'")
    assert(dist.label == "Multinomial | Multinomial")
  }

  "Multinomial_MultinomialParents.numberOfParameters" should "return the valid number of parameters of the distribution" in{

    Given("a multinomial variable with 4 parameters and a set of 2 multinomial parents with 3 and 8 parameters respectively")
    val variable = VariableFactory.newMultinomialVariable("multinomial",3)
    val parent1 = VariableFactory.newMultinomialVariable("mult1", 4)
    val parent2 = VariableFactory.newMultinomialVariable("mult2", 8)

    When("creating a Multinomial_MultinomialParents distribution from it")
    val dist = Multinomial_MultinomialParents(variable, Set(parent1, parent2))

    Then("this distribution should have 3*(4*8) parameters")
    assert(dist.numberOfParameters == 3*4*8)
  }

  /** These variable are all immutable objects with immutable references so they are safe to use in multiple tests */
  val mult_3states = VariableFactory.newMultinomialVariable("multinomial",3)
  val parent1_2states = VariableFactory.newMultinomialVariable("mult1", 2)
  val parent2_2states = VariableFactory.newMultinomialVariable("mult2", 2)
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
    val dist = Multinomial_MultinomialParents(mult_3states, Set(parent1_2states, parent2_2states), parameterizedDistributions)

    Then("getUnivariateDistribution(parent1_2states = 0, parent2_2states = 1) should return the multinomial distribution with parameters [0.5, 0.4, 0.1]")
    assert(dist.getUnivariateDistribution(assignments0_1).parameters equals Vector(0.5, 0.4, 0.1))

    And("getUnivariateDistribution(parent1_2states = 1, parent2_2states = 1) should return the multinomial distribution with parameters [0.01, 0.99, 0.0]")
    assert(dist.getUnivariateDistribution(assignments1_1).parameters equals Vector(0.01, 0.99, 0.0))

    And("getUnivariateDistribution(parent1_2states = -1, parent2_2states = 1) should throw an IllegalArgumentException")
    a[IllegalArgumentException] should be thrownBy {
      dist.getUnivariateDistribution(Assignments(Set(Assignment(parent1_2states, -1), Assignment(parent2_2states, 1))))
    }

    And("getUnivariateDistribution(parent1_2states = 99, parent2_2states = 1) should throw an IllegalArgumentException")
    a[IllegalArgumentException] should be thrownBy {
      dist.getUnivariateDistribution(Assignments(Set(Assignment(parent1_2states, 99), Assignment(parent2_2states, 1))))
    }

    And("getUnivariateDistribution(newVariable = 0, parent2_2states = 1) should throw an IllegalArgumentException")
    a[IllegalArgumentException] should be thrownBy {
      val newVariable = VariableFactory.newMultinomialVariable("newVariable", 2)
      dist.getUnivariateDistribution(Assignments(Set(Assignment(newVariable, 0), Assignment(parent2_2states, 1))))
    }

    And("getUnivariateDistribution(parent2_2states = 1) should throw an IllegalArgumentException")
    a[IllegalArgumentException] should be thrownBy {
      dist.getUnivariateDistribution(Assignments(Set(Assignment(parent2_2states,0))))
    }
  }

  "Multinomial_MultinomialParents.conditionalProbability(assignments, x)" should "return  P(X = x | assignments)" in{

    Given("a multinomial variable with 3 parameters and a set of 2 multinomial parents with 2 parameters each (manual parameters)")

    When("creating a Multinomial_MultinomialParents distribution from it")
    val dist = Multinomial_MultinomialParents(mult_3states, Set(parent1_2states, parent2_2states), parameterizedDistributions)

    Then("conditionalProbability(X = 1 | parent1_2states = 0, parent2_2states = 1) should return 0.4")
    assert(Utils.eqDouble(dist.conditionalProbability(assignments0_1, 1), 0.4))

    And("conditionalProbability(X = 2 | parent1_2states = 1, parent2_2states = 1) should return 0.0")
    assert(Utils.eqDouble(dist.conditionalProbability(assignments1_1, 2), 0.0))

    And("conditionalProbability(X = 6 | parent1_2states = 1, parent2_2states = 1) should throw and IllegalArgumentException (X = 6 is an invalid value)")
    a[IllegalArgumentException] should be thrownBy {
      dist.conditionalProbability(assignments1_1, 6)
    }
  }

  "Multinomial_MultinomialParents.logConditionalProbability(assignments, x)" should "return  log P(X = x | assignments)" in{

    Given("a multinomial variable with 3 parameters and a set of 2 multinomial parents with 2 parameters each (manual parameters)")

    When("creating a Multinomial_MultinomialParents distribution from it")
    val dist = Multinomial_MultinomialParents(mult_3states, Set(parent1_2states, parent2_2states), parameterizedDistributions)

    Then("conditionalProbability(X = 1 | parent1_2states = 0, parent2_2states = 1) should return 0.4")
    assert(Utils.eqDouble(dist.logConditionalProbability(assignments0_1, 1), FastMath.log(0.4)))

    And("conditionalProbability(X = 2 | parent1_2states = 1, parent2_2states = 1) should return 0.0")
    // -infinity
    assert(dist.logConditionalProbability(assignments1_1, 2) == FastMath.log(0.0))

    And("conditionalProbability(X = 6 | parent1_2states = 1, parent2_2states = 1) should throw and IllegalArgumentException (X = 6 is an invalid value)")
    a[IllegalArgumentException] should be thrownBy {
      dist.logConditionalProbability(assignments1_1, 6)
    }
  }

  "Multinomial_MultinomialParents.conditionalProbability(assignments, x0, x1)" should "return  log P(x0 < X <= x1 | assignments)" in{

    Given("a multinomial variable with 3 parameters and a set of 2 multinomial parents with 2 parameters each (manual parameters)")

    When("creating a Multinomial_MultinomialParents distribution from it")
    val dist = Multinomial_MultinomialParents(mult_3states, Set(parent1_2states, parent2_2states), parameterizedDistributions)

    Then("conditionalProbability(1 < X <= 2 | parent1_2states = 0, parent2_2states = 1) should return 0.1")
    assert(Utils.eqDouble(dist.conditionalProbability(assignments0_1, 1, 2), 0.1))

    And("conditionalProbability(-1 < X <= 2 | parent1_2states = 1, parent2_2states = 0) should throw and IllegalArgumentException (x0 = -1 is an invalid value)")
    a[IllegalArgumentException] should be thrownBy {
      println(dist.conditionalProbability(assignments1_0, -1, 2))
    }

    And("conditionalProbability(1 < X <= 0 | parent1_2states = 1, parent2_2states = 1) should throw and IllegalArgumentException (x0 > x1)")
    a[IllegalArgumentException] should be thrownBy {
      dist.conditionalProbability(assignments1_1, 1, 0)
    }
  }

  "Multinomial_MultinomialParents.cumulativeConditionalProbability(assignments, x)" should "return  P(X <= x | assignments)" in{

    Given("a multinomial variable with 3 parameters and a set of 2 multinomial parents with 2 parameters each (manual parameters)")

    When("creating a Multinomial_MultinomialParents distribution from it")
    val dist = Multinomial_MultinomialParents(mult_3states, Set(parent1_2states, parent2_2states), parameterizedDistributions)

    Then("cumulativeConditionalProbability(X <= 2 | parent1_2states = 1, parent2_2states = 1) should return 1.0")
    assert(Utils.eqDouble(dist.cumulativeConditionalProbability(assignments0_1, 2), 1.0))

    And("cumulativeConditionalProbability(X <= 7 | parent1_2states = 0, parent2_2states = 1) should throw an IllegalArgumentException")
    a[IllegalArgumentException] should be thrownBy {
      dist.logConditionalProbability(assignments0_1, 7)
    }
    And("cumulativeConditionalProbability(X <= -1 | parent1_2states = 1, parent2_2states = 0) should throw an IllegalArgumentException")
    a[IllegalArgumentException] should be thrownBy {
      dist.logConditionalProbability(assignments1_0, -1)
    }
  }

  "Multinomial_MultinomialParents.conditionalDensity(assignments, x)" should "" is pending

  "Multinomial_MultinomialParents.logConditionalDensity(assignments, x)" should "" is pending

}
