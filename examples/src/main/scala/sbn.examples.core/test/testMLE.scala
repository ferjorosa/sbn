package sbn.examples.core.test

import sbn.core.io.DataFileLoader
import sbn.core.learning.parametric.{MaximumLikelihood, ParameterLearningAlgorithm}

/**
  * Created by fer on 5/12/16.
  */
object testMLE {

  def main(args: Array[String]): Unit = {

    val dataSet = DataFileLoader.loadImmutableDataSet("datasets/ferjorosaData/sprinklerData300_oneVar.arff")

    val parameterLearningAlgorithm: ParameterLearningAlgorithm = new MaximumLikelihood

    parameterLearningAlgorithm.learn()

    println("Fin")

  }

}
