package sbn.core.data.attributes

import java.util.NoSuchElementException

import sbn.core.CustomSpec

class AttributesSpec extends CustomSpec{

  "Attributes constructor" should "throw an IllegalArgumentException if there are repeated attribute names" in {
    val manifestAttr1 = Attribute("manifestAttr1", RealStateSpace())
    val manifestAttr2 = Attribute("manifestAttr1", FiniteStateSpace(2))

    a[IllegalArgumentException] should be thrownBy{
      Attributes(List(manifestAttr1, manifestAttr2))
    }
  }

  "Attributes constructor" should "throw an IllegalArgumentException if there are order values out of bound" in {
    val manifestAttr0 = Attribute("manifestAttr0", RealStateSpace())
    val manifestAttr1 = Attribute("manifestAttr1", FiniteStateSpace(3))
    val manifestAttr2 = Attribute("manifestAttr2", RealStateSpace(0, 2))
    val manifestAttr3 = Attribute("manifestAttr3", FiniteStateSpace(2))

    // Size = 4
    val attributeList = List(manifestAttr0,manifestAttr1,manifestAttr2,manifestAttr3)
    // Size = 4, "8" is out of bounds
    val order = List(0,1,2,8)

    a[IllegalArgumentException] should be thrownBy{
      Attributes(attributeList, order)
    }
  }

  "Attributes constructor" should "throw an IllegalArgumentException if the sizes of attributeList and attributeOrder differ" in {
    val manifestAttr0 = Attribute("manifestAttr0", RealStateSpace())
    val manifestAttr1 = Attribute("manifestAttr1", FiniteStateSpace(3))
    val manifestAttr2 = Attribute("manifestAttr2", RealStateSpace(0, 2))
    val manifestAttr3 = Attribute("manifestAttr3", FiniteStateSpace(2))

    // Size = 4
    val attributeList = List(manifestAttr0,manifestAttr1,manifestAttr2,manifestAttr3)
    // Size = 3
    val order = List(0,1,2)

    a[IllegalArgumentException] should be thrownBy{
      Attributes(attributeList, order)
    }
  }

  "Attributes constructor" should "throw an IllegalArgumentException if there are repeated values in the attributeOrder collection" in {
    val manifestAttr0 = Attribute("manifestAttr0", RealStateSpace())
    val manifestAttr1 = Attribute("manifestAttr1", FiniteStateSpace(3))
    val manifestAttr2 = Attribute("manifestAttr2", RealStateSpace(0, 2))
    val manifestAttr3 = Attribute("manifestAttr3", FiniteStateSpace(2))

    // Size = 4
    val attributeList = List(manifestAttr0,manifestAttr1,manifestAttr2,manifestAttr3)
    // Size = 4, "2" is repeated
    val order = List(0,1,2,2)

    a[IllegalArgumentException] should be thrownBy{
      Attributes(attributeList, order)
    }
  }

  "Attributes.orderedAttributeList" should "return the attributeList following the specified order" in {
    val manifestAttr0 = Attribute("manifestAttr0", RealStateSpace())
    val manifestAttr1 = Attribute("manifestAttr1", FiniteStateSpace(3))
    val manifestAttr2 = Attribute("manifestAttr2", RealStateSpace(0, 2))
    val manifestAttr3 = Attribute("manifestAttr3", FiniteStateSpace(2))

    // Size = 4
    val attributeList = List(manifestAttr0,manifestAttr1,manifestAttr2,manifestAttr3)
    // Size = 4
    val order = List(3,0,1,2)

    val attributes = Attributes(attributeList, order)
    val manuallyOrderedAttributeList = List(manifestAttr3, manifestAttr0, manifestAttr1, manifestAttr2)
    val manuallyOrderedAttributes = Attributes(manuallyOrderedAttributeList)

    assert(attributes.orderedAttributeList equals manuallyOrderedAttributes.orderedAttributeList)
  }

  "Attributes.getAttributeByName" should "return the corresponding Attribute object if present" in {
    val manifestAttr0 = Attribute("manifestAttr0", RealStateSpace())
    val manifestAttr1 = Attribute("manifestAttr1", FiniteStateSpace(3))
    val attributeList = List(manifestAttr0,manifestAttr1)
    val attributes = Attributes(attributeList)

    val attribute: Attribute = attributes.getAttributeByName("manifestAttr1")

    assert(attribute equals manifestAttr1)
  }

  "Attributes.getAttributeByName" should "return 'None' if no Attribute object corresponds to the name" in {
    val manifestAttr0 = Attribute("manifestAttr0", RealStateSpace())
    val manifestAttr1 = Attribute("manifestAttr1", FiniteStateSpace(3))
    val attributeList = List(manifestAttr0,manifestAttr1)
    val attributes = Attributes(attributeList)

    a[NoSuchElementException] should be thrownBy {
      attributes.getAttributeByName("Attribute3")
    }

  }

  "Attributes.apply(index)" should "return the corresponding Attribute object if present" in {
    // With an order by default
    val manifestAttr0 = Attribute("manifestAttr0", RealStateSpace())
    val manifestAttr1 = Attribute("manifestAttr1", FiniteStateSpace(3))
    val manifestAttr2 = Attribute("manifestAttr2", RealStateSpace(0, 2))
    val manifestAttr3 = Attribute("manifestAttr3", FiniteStateSpace(2))

    val attributeList = List(manifestAttr0,manifestAttr1,manifestAttr2,manifestAttr3)
    val defaultOrderAttributes = Attributes(attributeList)

    assert(defaultOrderAttributes(2) equals manifestAttr2)

    // With a specific order
    val specificOrder = List(3,2,1,0)
    val specificOrderAttributes =  Attributes(attributeList, specificOrder)

    assert(specificOrderAttributes(0) equals manifestAttr3)

  }

  "Attributes.apply(index)" should "throw an IndexOutOfBoundsException if the index doesn't correspond to an Attribute object" in {
    val manifestAttr0 = Attribute("manifestAttr0", RealStateSpace())
    val manifestAttr1 = Attribute("manifestAttr1", FiniteStateSpace(3))
    val attributeList = List(manifestAttr0,manifestAttr1)
    val attributes = Attributes(attributeList)

    a[IndexOutOfBoundsException] should be thrownBy{
      attributes(2)
    }
  }

  "Attributes.size" should "return the contents size" in {
    val manifestAttr0 = Attribute("manifestAttr0", RealStateSpace())
    val manifestAttr1 = Attribute("manifestAttr1", FiniteStateSpace(3))
    val attributeList = List(manifestAttr0,manifestAttr1)
    val attributes = Attributes(attributeList)

    assert(attributeList.size ==  attributes.size)
  }

  "Attributes.iterator" should "work properly" in {
    val manifestAttr0 = Attribute("manifestAttr0", RealStateSpace())
    val manifestAttr1 = Attribute("manifestAttr1", FiniteStateSpace(3))
    val attributeList = List(manifestAttr0,manifestAttr1)
    val attributes = Attributes(attributeList)

    //Trying some of the Iterable[+T] methods
    assert(attributes.map(_.name).size == 2)
    assert(attributes.count(_.stateSpaceType.isInstanceOf[RealStateSpace]) == 1)
    assert(attributes.filter(_.name equals "attribute3").size == 0)
  }

  "Attributes.attributeIndexes" should "return a Map associating each attribute to it index" in {
    Given("a manually created Attributes object with 2 attributes")
    val attribute1 = Attribute("attribute1", RealStateSpace())
    val attribute2 = Attribute("attribute2", FiniteStateSpace(3))
    val attributes = Attributes(List(attribute1, attribute2))

    When("calling attributes.attributeIndexes")
    val attributeIndexMapping = attributes.attributeIndexes

    Then("a Map[Attribute, Int] of size 2 should be returned")
    assert(attributeIndexMapping.size == 2)
    assert(attributeIndexMapping(attribute1) == 0)
    assert(attributeIndexMapping(attribute2) == 1)
  }

  "Attributes.indexOf(attribute)" should "throw a NoSuchElementException if the attribute is not present" in {
    Given("a manually created Attributes object with 2 attributes")
    val attribute1 = Attribute("attribute1", RealStateSpace())
    val attribute2 = Attribute("attribute2", FiniteStateSpace(3))
    val attributes = Attributes(List(attribute1, attribute2))

    When("calling attributes.indexOf(attributeNotPresent)")
    val attributeNotPresent = Attribute("attributeNotPresent", RealStateSpace())

    Then("a NoSuchElementException should be thrown")
    a[NoSuchElementException] should be thrownBy{
      attributes.indexOf(attributeNotPresent)
    }
  }

  "Attributes.indexOf(attribute)" should "return the correct index if the attribute is present" in {
    Given("a manually created Attributes object with 2 attributes")
    val attribute1 = Attribute("attribute1", RealStateSpace())
    val attribute2 = Attribute("attribute2", FiniteStateSpace(3))
    val attributes = Attributes(List(attribute1, attribute2))

    When("calling attributes.indexOf(attribute1)")

    Then("0 should be returned")
    assert(attributes.indexOf(attribute1) == 0)
    assert(attributes.indexOf(attribute2) == 1)
  }

}
