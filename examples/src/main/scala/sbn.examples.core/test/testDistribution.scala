package sbn.examples.core.test

import breeze.linalg.DenseVector
import org.apache.commons.math3.util.FastMath
import sbn.core.statistics.distributions.{Gaussian, Multinomial}
import sbn.core.statistics.exponentialfamily.distributions.{EF_Gaussian, EF_Multinomial}
import sbn.core.variables.ModelVariablesFactory

/**
  * Created by fer on 1/12/16.
  */
object testDistribution {

  def main(args: Array[String]): Unit = {
    testMultinomial()
  }

  private def testGaussian(): Unit ={
    val ef_gaussian = EF_Gaussian(ModelVariablesFactory.newGaussianVariable(""), 0, 1)
    val gaussian = Gaussian(ModelVariablesFactory.newGaussianVariable("s"))

    println(FastMath.exp(gaussian.logDensity(0.5)))
    println(FastMath.exp(ef_gaussian.logDensity(0.5)))
  }

  private def testMultinomial(): Unit ={

    val ef_multinomial = EF_Multinomial(ModelVariablesFactory.newMultinomialVariable("", 3), Vector(0.5, 0.3, 0.2))
    val multinomial = Multinomial(ModelVariablesFactory.newMultinomialVariable("", 3), Vector(0.5, 0.3, 0.2))

    println(FastMath.exp(multinomial.logDensity(1)))
    println(FastMath.exp(ef_multinomial.logDensity(1)))
  }

}