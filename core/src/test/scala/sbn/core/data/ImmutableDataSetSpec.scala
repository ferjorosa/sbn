package sbn.core.data

import sbn.core.CustomSpec
import sbn.core.data.attributes.{Attribute, Attributes, FiniteStateSpace, RealStateSpace}
import sbn.core.utils.Utils

/**
  * Created by fer on 13/12/16.
  */
class ImmutableDataSetSpec extends CustomSpec{

  "ImmutableDataSet.dataMatrix" should "return a Vector[Vector[Double]] containing only its data values" in {

    Given("an ImmutableDataSet of 3 instances")
    val attribute1 = Attribute("attribute1", RealStateSpace())
    val attribute2 = Attribute("attribute2", FiniteStateSpace(3))
    val attributes = Attributes(List(attribute1, attribute2))

    val instance1 = DataInstance(attributes, Vector(1.77, 2))
    val instance2 = DataInstance(attributes, Vector(2.77, 1))
    val instance3 = DataInstance(attributes, Vector(3.77, 0))

    val dataSet = ImmutableDataSet("test", attributes, Vector(instance1, instance2, instance3))

    When("calling dataSet.dataMatrix")
    val dataMatrix = dataSet.dataMatrix

    Then("a Vector[Vector[Double]] of size 3 with its contents should be returned")
    assert(Utils.eqDouble(dataMatrix(0)(0), 1.77))
    assert(Utils.eqDouble(dataMatrix(1)(0), 2.77))
    assert(Utils.eqDouble(dataMatrix(2)(1), 0))
  }

}
