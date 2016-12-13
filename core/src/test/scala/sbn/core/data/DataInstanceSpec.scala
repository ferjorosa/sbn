package sbn.core.data

import sbn.core.CustomSpec
import sbn.core.data.attributes.{Attribute, Attributes, FiniteStateSpace, RealStateSpace}
import sbn.core.utils.Utils

class DataInstanceSpec extends CustomSpec {

  "DataInstance.value(attribute)" should "throw an NoSuchElementException if the attribute doesn't belong to its Attributes" in {

    Given("a  manually created DataInstance with a manually created Attributes object")
    val attribute1 = Attribute("attribute1", RealStateSpace())
    val attribute2 = Attribute("attribute2", FiniteStateSpace(3))
    val attributes = Attributes(List(attribute1, attribute2))
    val instance = DataInstance(attributes, Vector(3.77, 2))

    When("trying to get a value with an Attribute not present in Attributes")
    val attributeNotPresent = Attribute("attributeNotPresent", FiniteStateSpace(2))

    Then("an NoSuchElementException is thrown")
    a[NoSuchElementException] should be thrownBy{
      instance.value(attributeNotPresent)
    }
  }

  it should "return the correct value if the attribute is present in its associated Attributes object" in {

    Given("a  manually created DataInstance with a manually created Attributes object of size 2")
    val attribute1 = Attribute("attribute1", RealStateSpace())
    val attribute2 = Attribute("attribute2", FiniteStateSpace(3))
    val attributes = Attributes(List(attribute1, attribute2))
    val instance = DataInstance(attributes, Vector(3.77, 2))

    When("trying to get the value of the first one")

    Then("it should return 3.77")
      assert(Utils.eqDouble(instance.value(attribute1), 3.77))
  }

  "DataInstanceFactory.fromARFFDataLine" should "return a Success[DataInstance] if the line is correctly formatted" in {
    val finiteAttr1 = Attribute("finiteAttr1", FiniteStateSpace(2, Vector("0", "1"), Map("0" -> 0, "1" -> 1)))
    val finiteAttr2 = Attribute("finiteAttr2", FiniteStateSpace(4))
    val realAttr1 = Attribute("realAttr1", RealStateSpace())
    val realAttr2 = Attribute("realAttr2", RealStateSpace())

    val finiteAttributes = Attributes(List(finiteAttr1, finiteAttr2))
    val realAttributes = Attributes(List(realAttr1, realAttr2))
    val allAttributes = Attributes(List(finiteAttr1, finiteAttr2, realAttr1, realAttr2))

    // ARFF data lines
    val finiteStateSpaceLine = "0, s3" // s3 is the name by default for the state 3 of a 4-state finiteStateSpace
    val realStateSpaceLine = "0.56, 156.38"
    val mixedStateSpacesLine = "1, s2, 7.94, 1000"

    val finiteDataInstance = DataInstanceFactory.fromARFFDataLine(finiteAttributes,finiteStateSpaceLine)
    assert(finiteDataInstance.get.attributes equals finiteAttributes)
    assert(finiteDataInstance.get.values equals Vector(0, 3))
    assert(finiteDataInstance.get.values equals Vector(0.0, 3.0))

    val realDataInstance = DataInstanceFactory.fromARFFDataLine(realAttributes,realStateSpaceLine)
    assert(realDataInstance.get.attributes equals realAttributes)
    assert(realDataInstance.get.values equals Vector(0.56, 156.38))

    val mixedDataInstance = DataInstanceFactory.fromARFFDataLine(allAttributes, mixedStateSpacesLine)
    assert(mixedDataInstance.get.attributes equals allAttributes)
    assert(mixedDataInstance.get.values equals Vector(1, 2, 7.94, 1000))
  }

  it should "return a Failure if the line is badly formatted" in {
    val finiteAttr1 = Attribute("finiteAttr1", FiniteStateSpace(2, Vector("0", "1"), Map("0" -> 0, "1" -> 1)))
    val finiteAttr2 = Attribute("finiteAttr2", FiniteStateSpace(2))
    val realAttr1 = Attribute("realAttr1", RealStateSpace())
    val realAttr2 = Attribute("realAttr2", RealStateSpace())

    val finiteAttributes = Attributes(List(finiteAttr1, finiteAttr2))
    val realAttributes = Attributes(List(finiteAttr1, finiteAttr2))
    val allAttributes = Attributes(List(finiteAttr1, finiteAttr2, realAttr1, realAttr2))

    // Mixing real values with finiteStateSpace attributes. It will return a failure because these values are keys in
    // the FiniteStateSpace mapping
    val mixedStateSpacesLine1 = "s0, 5.89"
    val mixedStateSpacesLine2 = "0.0, 1.0"
    assert (DataInstanceFactory.fromARFFDataLine(finiteAttributes,mixedStateSpacesLine1).isFailure)
    assert (DataInstanceFactory.fromARFFDataLine(finiteAttributes,mixedStateSpacesLine2).isFailure)

    // Mixing numbers with pure strings
    val badlyFormattedLine1 = "hola, pedro"
    val badlyFormattedLine2 = "line, 0"
    assert (DataInstanceFactory.fromARFFDataLine(finiteAttributes,badlyFormattedLine1).isFailure)
    assert (DataInstanceFactory.fromARFFDataLine(finiteAttributes,badlyFormattedLine2).isFailure)
  }

  it should "return a Failure if number of columns does not match the number of attributes" in {
    val attr1 = Attribute("attr1", FiniteStateSpace(2))
    val attributes = Attributes(List(attr1))

    val manualArffDataLine = "s0, s1, s0, s0"
    assert (DataInstanceFactory.fromARFFDataLine(attributes,manualArffDataLine).isFailure)
  }
}
