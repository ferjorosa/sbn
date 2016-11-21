package sbn.core.distributions

import sbn.core.CustomSpec
import sbn.core.variables.VariableFactory

class GaussianSpec extends CustomSpec{

  "Gaussian constructor" should "throw an IllegalArgumentException if the variable's distribution type is not Gaussian" in {
    a[IllegalArgumentException] should be thrownBy {
      Gaussian(VariableFactory.newMultinomialVariable("star wars", 2), 2.54, 0.64)
    }
  }

  "Gaussian constructor" should "throw an IllegalArgumentException if the variance <= 0" in {
    a[IllegalArgumentException] should be thrownBy {
      Gaussian(VariableFactory.newGaussianVariable("gaussian_noBounds"), 5, -0.0001)
    }
  }

  "Gaussian.apply" should "create a standard Gaussian distribution with mean = 0 and variance = 1" in {
    val dist = Gaussian(VariableFactory.newGaussianVariable("gaussian", -2, 2))

    assert(dist.mean == 0)
    assert(dist.variance == 1)
  }

  "Gaussian.label" should "return 'Gaussian'" in {
    assert(Gaussian(VariableFactory.newGaussianVariable("gaussian_noBounds")).label == "Gaussian")
  }

  "Gaussian.numberOfParameters" should "be == 2" in {
    assert(Gaussian(VariableFactory.newGaussianVariable("gaussian_noBounds")).numberOfParameters == 2)
  }

  "Gaussian.parameters" should "be a Vector containing the mean and variance values" in {
    assert(Gaussian(VariableFactory.newGaussianVariable("gaussian_noBounds")).parameters == Vector(0, 1))
  }

  "Gaussian.getLogProbability" should "hoola-hoop" in {
    val dist = Gaussian(VariableFactory.newGaussianVariable("gaussian", 0, 1))

    val now1 = System.nanoTime()
    for(i<-0 until 1000000)
      dist.logProbability(0.33)
    val end1 = System.nanoTime()

    val now2 = System.nanoTime()
    for(i<-0 until 1000000)
      dist.logProbability(0.33)
    val end2 = System.nanoTime()

    val temp1 = end1 - now1
    val temp2 = end2 - now2

    println(end1 -now1, end2 - now2)

    if(temp1 > temp2)
      println("temp1 mas lento por "+ (temp1 -temp2) + "ns")

    if(temp2 > temp1)
      println("temp2 mas lento por "+ (temp2 -temp1) + "ns")
  }

}
