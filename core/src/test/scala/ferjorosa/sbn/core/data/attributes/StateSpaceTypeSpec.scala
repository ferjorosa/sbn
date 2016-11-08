package ferjorosa.sbn.core.data.attributes

import ferjorosa.sbn.core.CustomSpec

class StateSpaceTypeSpec extends CustomSpec{

  "FiniteStateSpace constructor" should "throw IllegalArgumentException if the number of states is <= 0" in {
    a[IllegalArgumentException] should be thrownBy{
      FiniteStateSpace(0)
    }
  }

  "FiniteStateSpace constructor" should "throw IllegalArgumentException if stateNames.size != numberOfStates" in {
    a[IllegalArgumentException] should be thrownBy{
      FiniteStateSpace(1, Vector("s0", "s1"), Map("s0" -> 0, "s1" -> 1))
    }
  }

  "FiniteStateSpace constructor" should "throw IllegalArgumentException if mapStatesNames.keys.size != numberOfStates" in {
    a[IllegalArgumentException] should be thrownBy{
      FiniteStateSpace(2,
        Vector("s0", "s1"), // size 2
        Map("s0" -> 0)) // size 1
    }
  }

  "FiniteStateSpace constructor" should "throw IllegalArgumentException if there are repeated mapStateNames values" in {
    a[IllegalArgumentException] should be thrownBy{
      FiniteStateSpace(2, Vector("s0", "s1"), Map("s0" -> 0, "s1" -> 0))
    }
  }

  "FiniteStateSpace.getIndexOfState" should "throw a NoSuchElementException if the passed name is incorrect" in {
    val finite = FiniteStateSpace(Vector("state1", "estado2", "s3"))
    val finite2 = FiniteStateSpace(4)

    a[NoSuchElementException] should be thrownBy{finite.getIndexOfState("sssss")}
    a[NoSuchElementException] should be thrownBy{finite2.getIndexOfState("ssss")}
  }

  "FiniteStateSpace.getIndexOfState" should "return the index of the state if the passed name is correct" in {
    val finite = FiniteStateSpace(Vector("state1", "estado2", "s3"))
    val finite2 = FiniteStateSpace(4)

    assert(finite.getIndexOfState("estado2") == 1)
    assert(finite2.getIndexOfState("s2") == 2)
  }

}
