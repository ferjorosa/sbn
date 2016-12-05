package sbn.core.models

import breeze.linalg.DenseVector
import sbn.core.models.graph.DirectedGraph
import sbn.core.statistics.exponentialfamily.distributions.EF_Distribution
import sbn.core.variables.MainVariable

/**
  * Created by fer on 5/12/16.
  */
case class EF_BayesianNetwork(name: String,
                              dag: DirectedGraph[MainVariable],
                              distributions: Seq[EF_Distribution],
                              momentParameters: Vector[DenseVector[Double]]) {

  val naturalParameters: Vector[DenseVector[Double]] = ???

  def sufficientStatistics(x: Double): Vector[DenseVector[Double]] = ???
}

object EF_BayesianNetwork {

  def apply(name: String,
            dag: DirectedGraph[MainVariable],
            distributions: Seq[EF_Distribution]): EF_BayesianNetwork =
  // TODO: deberia ser zeroSuffStatistics o mejor aun los parametros aleatorios otorgados al generar las dist
    EF_BayesianNetwork(name, dag, distributions, null)
}
