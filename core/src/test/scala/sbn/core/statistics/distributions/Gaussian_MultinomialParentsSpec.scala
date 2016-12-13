package sbn.core.statistics.distributions

import org.apache.commons.math3.util.FastMath
import sbn.core.CustomSpec
import sbn.core.utils.Utils
import sbn.core.variables.model.ModelVariablesFactory
import sbn.core.variables.{Assignment, Assignments}

class Gaussian_MultinomialParentsSpec extends CustomSpec{

  "Gaussian_MultinomialParents.apply" should "throw an IllegalArgumentException if the variable is not of GaussianType" in{

    Given("a variable of Multinomial type and a set of multinomial parents")
    val variable = ModelVariablesFactory.newMultinomialLV("multinomial", 15)
    val parent1 = ModelVariablesFactory.newMultinomialLV("mult1", 2)
    val parent2 = ModelVariablesFactory.newMultinomialLV("mult2", 3)

    When("creating a Gaussian_MultinomialParents distribution from it")

    Then("a IllegalArgumentException should be thrown")
    a[IllegalArgumentException] should be thrownBy {
      Gaussian_MultinomialParents(variable, Set(parent1, parent2))
    }
  }

  it should "throw an IllegalArgumentException if parents are not exclusively of MultinomialType" in {

    Given("a variable of Gaussian type and a set of mixed-type parents")
    val variable = ModelVariablesFactory.newGaussianLV("gaussian")
    val parent1 = ModelVariablesFactory.newGaussianLV("gaussianP")
    val parent2 = ModelVariablesFactory.newMultinomialLV("multinomialP", 3)

    When("creating a Gaussian_MultinomialParents distribution from it")

    Then("a IllegalArgumentException should be thrown")
    a[IllegalArgumentException] should be thrownBy {
      Gaussian_MultinomialParents(variable, Set(parent1, parent2))
    }
  }

  "Gaussian_MultinomialParents.label" should "return 'Gaussian | Multinomial'" in {

    Given("a variable of gaussian type and a set of multinomial parents")
    val variable = ModelVariablesFactory.newGaussianLV("gaussian")
    val parent1 = ModelVariablesFactory.newMultinomialLV("mult1", 1)
    val parent2 = ModelVariablesFactory.newMultinomialLV("mult2", 4)

    When("creating a Gaussian_MultinomialParents distribution from it")
    val dist = Gaussian_MultinomialParents(variable, Set(parent1, parent2))

    Then("its label must be 'Gaussian | Multinomial'")
    assert(dist.label == "Gaussian | Multinomial")
  }

  "Gaussian_MultinomialParents.numberOfParameters" should "return the valid number of parameters of the distribution" in{

    Given("a standard gaussian variable (always 2 parameters, the mean and variance) and a set of 2 multinomial parents with 3 and 8 parameters respectively")
    val variable = ModelVariablesFactory.newGaussianLV("")
    val parent1 = ModelVariablesFactory.newMultinomialLV("mult1", 3)
    val parent2 = ModelVariablesFactory.newMultinomialLV("mult2", 8)

    When("creating a Gaussian_MultinomialParents distribution from it")
    val dist = Gaussian_MultinomialParents(variable, Set(parent1, parent2))

    Then("this distribution should have 2*(3*8) parameters")
    assert(dist.numberOfParameters == 2*3*8)
  }

  /** These variable are all immutable objects with immutable references so they are safe to use in multiple tests */
  val st_gaussian = ModelVariablesFactory.newGaussianLV("st_gaussian")
  val parent1_2states = ModelVariablesFactory.newMultinomialLV("mult1", 2)
  val parent2_2states = ModelVariablesFactory.newMultinomialLV("mult2", 2)
  // Manually created distributions
  val parent1_0_parent2_0 = Gaussian(st_gaussian, 0, 1)
  val parent1_0_parent2_1 = Gaussian(st_gaussian,-5, 25)
  val parent1_1_parent2_0 = Gaussian(st_gaussian, -100, 9)
  val parent1_1_parent2_1 = Gaussian(st_gaussian, 0, 16)
  //Manually created assignments (they are usually created automatically with Utils.cartesianProduct)
  val assignments0_0 = Assignments(Set(Assignment(parent1_2states,0), Assignment(parent2_2states,0)))
  val assignments0_1 = Assignments(Set(Assignment(parent1_2states,0), Assignment(parent2_2states,1)))
  val assignments1_0 = Assignments(Set(Assignment(parent1_2states,1), Assignment(parent2_2states,0)))
  val assignments1_1 = Assignments(Set(Assignment(parent1_2states,1), Assignment(parent2_2states,1)))
  // Manually assign each distribution to its parent assignment
  val parameterizedDistributions = Map(assignments0_0 -> parent1_0_parent2_0,
                                       assignments0_1 -> parent1_0_parent2_1,
                                       assignments1_0 -> parent1_1_parent2_0,
                                       assignments1_1 -> parent1_1_parent2_1)

  "Gaussian_MultinomialParents.getUnivariateDistribution" should "" in {

    Given("a gaussian variable and a set of 2 multinomial parents with 2 parameters each (manual parameters)")

    When("creating a specific Gaussian_MultinomialParents distribution from it")
    val dist = Gaussian_MultinomialParents(st_gaussian, Set(parent1_2states, parent2_2states), parameterizedDistributions)

    Then("getUnivariateDistribution(parent1_2states = 0, parent2_2states = 1) should return the gaussian distribution with mean = -5 & variance = 5")
    assert(dist.getUnivariateDistribution(assignments0_1).parameters equals Vector(-5, 25))

    And("getUnivariateDistribution(parent1_2states = 1, parent2_2states = 1) should return the gaussian distribution with mean = 0 & variance = 3")
    assert(dist.getUnivariateDistribution(assignments1_1).parameters equals Vector(0, 16))

    And("getUnivariateDistribution(parent1_2states = -1, parent2_2states = 1) should throw an NoSuchElementException")
    a[NoSuchElementException] should be thrownBy {
      dist.getUnivariateDistribution(Assignments(Set(Assignment(parent1_2states, -1), Assignment(parent2_2states, 1))))
    }

    And("getUnivariateDistribution(newVariable = 0, parent2_2states = 1) should throw an NoSuchElementException")
    a[NoSuchElementException] should be thrownBy {
      val newVariable = ModelVariablesFactory.newMultinomialLV("newVariable", 2)
      dist.getUnivariateDistribution(Assignments(Set(Assignment(newVariable, 0), Assignment(parent2_2states,1))))
    }

    And("getUnivariateDistribution(parent2_2states = 1) should throw an NoSuchElementException")
    a[NoSuchElementException] should be thrownBy {
      dist.getUnivariateDistribution(Assignments(Set(Assignment(parent2_2states,0))))
    }
  }

  "Gaussian_MultinomialParents.conditionalProbability" should "return  P(X = x | assignments)" in{

    Given("a gaussian variable and a set of 2 multinomial parents with 2 parameters each (manual parameters)")

    When("creating a specific Gaussian_MultinomialParents distribution from it")
    val dist = Gaussian_MultinomialParents(st_gaussian, Set(parent1_2states, parent2_2states), parameterizedDistributions)

    Then("conditionalProbability(X = 1.5 | parent1_2states = 0, parent2_2states = 1) should return 0.0")
    assert(Utils.eqDouble(dist.conditionalProbability(assignments0_1, 1.5), 0))

    And("conditionalProbability(X = 2 | parent1_2states = 1, parent2_2states = 1) should return 0.0")
    assert(Utils.eqDouble(dist.conditionalProbability(assignments1_1, -2.75), 0))
  }

  "Gaussian_MultinomialParents.logConditionalProbability" should "return  log P(X = x | assignments)" in{

    Given("a gaussian variable and a set of 2 multinomial parents with 2 parameters each (manual parameters)")

    When("creating a specific Gaussian_MultinomialParents distribution from it")
    val dist = Gaussian_MultinomialParents(st_gaussian, Set(parent1_2states, parent2_2states), parameterizedDistributions)

    Then("logConditionalProbability(X = 1.5 | parent1_2states = 0, parent2_2states = 1) should return -infinity")
    assert(dist.logConditionalProbability(assignments0_1, 1.5) == FastMath.log(0))

    And("logConditionalProbability(X = 2 | parent1_2states = 1, parent2_2states = 1) should return -infinity")
    assert(dist.logConditionalProbability(assignments1_1, -2.75) == FastMath.log(0))
  }

  "Gaussian_MultinomialParents.conditionalProbability(assignments, x0, x1)" should "return  log P(x0 < X <= x1 | assignments)" in{

    Given("a gaussian variable and a set of 2 multinomial parents with 2 parameters each (manual parameters)")

    When("creating a Gaussian_MultinomialParents distribution from it")
    val dist = Gaussian_MultinomialParents(st_gaussian, Set(parent1_2states, parent2_2states), parameterizedDistributions)

    Then("conditionalProbability(1 < X <= 2 | parent1_2states = 0, parent2_2states = 1) must equal cumulativeProbability(assignments0_1, 2) - cumulativeProbability(assignments0_1, 1)")
    assert(Utils.eqDouble(
      dist.conditionalProbability(assignments0_1, 1, 2),
      dist.cumulativeConditionalProbability(assignments0_1, 2) - dist.cumulativeConditionalProbability(assignments0_1, 1))
    )

    And("conditionalProbability(-101 < X <= -92 | parent1_2states = 1, parent2_2states = 0) should return ")
    assert(Utils.eqDouble(
      dist.conditionalProbability(assignments1_0, -101, -92),
      dist.cumulativeConditionalProbability(assignments1_0, -92) - dist.cumulativeConditionalProbability(assignments1_0, -101))
    )
  }

  "Gaussian_MultinomialParents.cumulativeConditionalProbability(assignments, x)" should "return  P(X <= x | assignments)" in{

    Given("a gaussian variabl and a set of 2 multinomial parents with 2 parameters each (manual parameters)")

    When("creating a Gaussian_MultinomialParents distribution from it")
    val dist = Gaussian_MultinomialParents(st_gaussian, Set(parent1_2states, parent2_2states), parameterizedDistributions)

    Then("cumulativeConditionalProbability(X <= 2 | parent1_2states = 0, parent2_2states = 1) must equal 0.91924 with an epsilon of 0.00001")
    println(dist.cumulativeConditionalProbability(assignments0_1, 2))
    assert(Utils.eqDouble(dist.cumulativeConditionalProbability(assignments0_1, 2), 0.91924, 0.00001))

    And("cumulativeConditionalProbability(X <= 2 | parent1_2states = 0, parent2_2states = 0) must equal 0.95818 with an epsilon of 0.00001")
    assert(Utils.eqDouble(dist.cumulativeConditionalProbability(assignments0_0, 1.73), 0.95818, 0.00001))

    And("cumulativeConditionalProbability(X <= -101 | parent1_2states = 1, parent2_2states = 0) must equal 0.36944 with an epsilon of 0.00001")
    assert(Utils.eqDouble(dist.cumulativeConditionalProbability(assignments1_0, -101), 0.36944, 0.00001))
  }

  "Gaussian_MultinomialParents.conditionalDensity" should "return pdf(X = x | assignments)" in {

    Given("a gaussian variable and a set of 2 multinomial parents with 2 parameters each (manual parameters)")

    When("creating a Gaussian_MultinomialParents distribution from it")
    val dist = Gaussian_MultinomialParents(st_gaussian, Set(parent1_2states, parent2_2states), parameterizedDistributions)

    Then("conditionalDensity(assignments0_1, -0.5) must equal 0.05321 with an epsilon of 0.0001")
    assert(Utils.eqDouble(dist.conditionalDensity(assignments0_1, -0.5), 0.05321, 0.0001))

    Then("conditionalDensity(assignments1_0, -93) must equal 0.00874 with an epsilon of 0.0001")
    assert(Utils.eqDouble(dist.conditionalDensity(assignments1_0, -93), 0.00874, 0.0001))
  }

  "Gaussian_MultinomialParents.logConditionalDensity" should "return log pdf(X = x | assignments)" in{

    Given("a gaussian variable and a set of 2 multinomial parents with 2 parameters each (manual parameters)")

    When("creating a Gaussian_MultinomialParents distribution from it")
    val dist = Gaussian_MultinomialParents(st_gaussian, Set(parent1_2states, parent2_2states), parameterizedDistributions)

    Then("logConditionalDensity(assignments0_1, -0.5) must equal log(0.05321) with an epsilon of 0.001")
    // error adds up, so the epsilon has to be reduced even more
    assert(Utils.eqDouble(dist.logConditionalDensity(assignments0_1, -0.5), FastMath.log(0.05321), 0.001))

    Then("logConditionalDensity(assignments1_0, -93) must equal log(0.00874) with an epsilon of 0.001")
    // error adds up, so the epsilon has to be reduced even more
    assert(Utils.eqDouble(dist.logConditionalDensity(assignments1_0, -93), FastMath.log(0.00874), 0.001))
  }

  "Gaussian_MultinomialParents.toEF_Distribution" should "return an equivalent EF_Gaussian_Multinomial object" in {

    Given("a gaussian variable and a set of 2 multinomial parents with 2 parameters each (manual parameters)")

    When("creating a Gaussian_MultinomialParents distribution from it")
    val dist = Gaussian_MultinomialParents(st_gaussian, Set(parent1_2states, parent2_2states), parameterizedDistributions)

    Then("dist.toEF_Distribution should return an equivalent EF_Gaussian_Multinomial object")
    val ef_dist = dist.toEF_Distribution
    assert(dist.variable equals ef_dist.variable)
    assert(dist.multinomialParents equals ef_dist.parents)

    val distAssignments = dist.assignedDistributions.keys
    val ef_distAssignments = ef_dist.assignedDistributions

    assert(distAssignments.size == ef_distAssignments.size)
    // Compare both distributions probabilities (their moment parameters)
    distAssignments.foreach{x =>
      assert(
        (dist.assignedDistributions(x).mean
          equals
          ef_dist.assignedDistributions(x).mean)
        &&
        (dist.assignedDistributions(x).variance
          equals
          ef_dist.assignedDistributions(x).variance)
      )}
  }

}
