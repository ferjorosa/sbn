package sbn.core.statistics.distributions

import sbn.core.CustomSpec
import sbn.core.data.attributes.FiniteStateSpace
import sbn.core.variables._
import sbn.core.variables.model.{ModelVariable, ModelVariablesFactory}

/**
  * Created by fer on 23/11/16.
  */
class BaseDistribution_MultinomialParentsSpec extends CustomSpec{

  "BaseDistribution_MultinomialParents.generateAssignmentsCombinations" should "" in{

    Given("a multinomial variable (3 params) and a set of 3 multinomial parents with a number of 2, 2, 3 parameters respectively ")
    val parent_1 = ModelVariablesFactory.newMultinomialLV("parent_1", 2)
    val parent_2 = ModelVariablesFactory.newMultinomialLV("parent_2", 2)
    val parent_3 = ModelVariablesFactory.newMultinomialLV("parent_3", 3)
    val variable = ModelVariablesFactory.newMultinomialLV("variable", 3)
    val parents: Vector[ModelVariable] = Vector(parent_1, parent_2, parent_3)

    When("calling generateAssingmentsCombinations(parents)")
    val combinations = BaseDistribution_Multinomial.generateAssignmentCombinations(parents)

    Then("the returned set of combinations should have size 2*2*3")
    assert(combinations.size == 2*2*3)
    assert(2*2*3 == parents.map(_.attribute.stateSpaceType.asInstanceOf[FiniteStateSpace].numberOfStates).product)

    And("combinations(8) should return [parent_1 = , parent_2 = , parent_3 = ]")
    assert(combinations(8) == Assignments(Set(Assignment(parent_1, 1), Assignment(parent_2, 0), Assignment(parent_3, 2))))
  }
}
