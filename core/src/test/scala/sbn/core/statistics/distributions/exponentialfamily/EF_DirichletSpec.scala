package sbn.core.statistics.distributions.exponentialfamily

import sbn.core.CustomSpec
import sbn.core.statistics.distributions.learning.EF_Dirichlet
import sbn.core.variables.model.ModelVariablesFactory

/**
  * Created by fer on 14/12/16.
  */
class EF_DirichletSpec extends CustomSpec{

  "EF_Dirichlet constructor" should "throw a RuntimeException if variable.distributionType is not DirichletType" in {

    Given("a variable of Gaussian type")
    val variable = ModelVariablesFactory.newGaussianLV("gaussian")

    When("Creating a EF_Dirichlet distribution from it")
    val concentrationParameters = Vector(1.0, 0.55)

    Then("a RuntimeException should be thrown")
    a[RuntimeException] should be thrownBy {
      EF_Dirichlet(variable, concentrationParameters)
    }
  }

  it should "throw a RuntimeException if the numberOfStates is < 2" in {

    val concentrationParameters = Vector(1.0)

    a[RuntimeException] should be thrownBy {
      EF_Dirichlet(ModelVariablesFactory.newDirichletLV("dirichlet", 1), concentrationParameters)
    }
  }

  it should "throw a RuntimeException if a concentration parameter is <= 0" in {

    val concentrationParameters = Vector(1.0, 0.0)

    a[RuntimeException] should be thrownBy {
      EF_Dirichlet(ModelVariablesFactory.newDirichletLV("dirichlet", 2), concentrationParameters)
    }
  }

}
