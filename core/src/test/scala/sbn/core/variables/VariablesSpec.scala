package sbn.core.variables

import sbn.core.CustomSpec
import sbn.core.data.attributes.RealStateSpace
import sbn.core.variables.model.{ModelVariable, ModelVariablesFactory}

/**
  * Created by fer on 19/12/16.
  */
class VariablesSpec extends CustomSpec{

  "Variables.constructor" should "create a Variables[V <: Variable] object if a correct Map[String, V <: Variable] is provided" in {

    Given("a Map[String, ModelVariable] of size 2")
    val multinomialVar = ModelVariablesFactory.newMultinomialLV("multinomial", 3)
    val gaussianVar = ModelVariablesFactory.newGaussianLV("gaussian")
    val variableCollection = Set(multinomialVar, gaussianVar)
    val variablesMap = variableCollection.map(v => (v.name, v)).toMap

    When("used as input for the Variables constructor")
    val variables = Variables(variablesMap)

    Then("a new Variables[ModelVariable] object should be created")
    assert(!variables.exists(!_.isInstanceOf[ModelVariable]))
  }

  it should "create a Variables object if a correct Set[V <: variable] is provided" in {

    Given("a Set[ModelVariable] of size 2")
    val multinomialVar = ModelVariablesFactory.newMultinomialLV("multinomial", 3)
    val gaussianVar = ModelVariablesFactory.newGaussianLV("gaussian")
    val variableSet = Set(multinomialVar, gaussianVar)

    When("used as input for the Variables constructor")
    val variables = Variables(variableSet)

    Then("a new Variables[ModelVariable] object should be created")
    assert(!variables.exists(!_.isInstanceOf[ModelVariable]))
  }

  "Variables.apply(variableName)" should "return a V <: Variable object if the name corresponds to an available variable" in {

    Given("Variables[ModelVariable] of size 3")
    val multinomialVar = ModelVariablesFactory.newMultinomialLV("multinomial", 3)
    val gaussianVar = ModelVariablesFactory.newGaussianLV("gaussian")
    val gammaVar = ModelVariablesFactory.newGammaLV("gamma")
    val variables = Variables(Set(multinomialVar, gaussianVar, gammaVar))

    When("calling Variables.apply(variableName) with an available variable name")
    val variable = variables("multinomial")

    Then("its associate variable object should be returned")
    assert(multinomialVar equals variable)
  }

  it should "throw a RuntimeException if the name doesn't exists" in {

    Given("Variables[ModelVariable] of size 3")
    val multinomialVar = ModelVariablesFactory.newMultinomialLV("multinomial", 3)
    val gaussianVar = ModelVariablesFactory.newGaussianLV("gaussian")
    val gammaVar = ModelVariablesFactory.newGammaLV("gamma")
    val variables = Variables(Set(multinomialVar, gaussianVar, gammaVar))

    When("calling Variables.apply(variableName) with an available variable name")

    Then("a exception should be thrown")
    a[RuntimeException] should be thrownBy {
      variables("dirichlet")
    }
  }

  "Variables.iterator" should "work properly" in {

    Given("Variables[ModelVariable] of size 3")
    val multinomialVar = ModelVariablesFactory.newMultinomialLV("multinomial", 3)
    val gaussianVar = ModelVariablesFactory.newGaussianLV("gaussian")
    val gammaVar = ModelVariablesFactory.newGammaLV("gamma")
    val variables = Variables(Set(multinomialVar, gaussianVar, gammaVar))

    Then("some of the Iterable[+T] methods should work properly")
    assert(variables.map(_.id).size == 3)
    assert(variables.count(_.attribute.stateSpaceType.isInstanceOf[RealStateSpace]) == 2)
  }
}
