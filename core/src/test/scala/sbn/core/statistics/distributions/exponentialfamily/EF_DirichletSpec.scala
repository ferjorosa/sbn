package sbn.core.statistics.distributions.exponentialfamily

import sbn.core.CustomSpec
import sbn.core.utils.Utils
import sbn.core.variables.model.ModelVariablesFactory

/**
  * Created by fer on 14/12/16.
  */
class EF_DirichletSpec extends CustomSpec{

  "EF_Dirichlet constructor" should "throw a RuntimeException if variable.distributionType is not DirichletType" in {

    Given("a variable of Gaussian type")
    val variable = ModelVariablesFactory.newGaussianLV("gaussian")

    When("Creating a EF_Dirichlet distribution from it")

    Then("a RuntimeException should be thrown")
    a[RuntimeException] should be thrownBy {
      EF_Dirichlet(variable, 2)
    }
  }

  it should "throw a RuntimeException if the numberOfStates is < 2" in {
    a[RuntimeException] should be thrownBy {
      EF_Dirichlet(ModelVariablesFactory.newDirichletLV("dirichlet", 1), 4.65)
    }
  }

  it should "throw a RuntimeException if the scale < 1.0" in {
    a[RuntimeException] should be thrownBy {
      EF_Dirichlet(ModelVariablesFactory.newDirichletLV("dirichlet", 2), 0.99)
    }
  }

  "EF_Dirichlet.apply" should "create a EF_Dirichlet distribution with a scale of 2 by default" in {

    Given("a dirichlet variable with 3 states")
    val variable = ModelVariablesFactory.newDirichletLV("dirichlet", 3)

    When("creating an EF_Dirichlet distribution from it")
    val distribution = EF_Dirichlet(variable)

    And("The scale of the distribution should be 2.0")
    assert(Utils.eqDouble(distribution.scale, 2.0))
  }

}
