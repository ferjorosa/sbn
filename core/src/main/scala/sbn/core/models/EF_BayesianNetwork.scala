package sbn.core.models

import sbn.core.models.graph.DirectedGraph
import sbn.core.statistics.exponentialfamily.distributions.EF_Distribution
import sbn.core.variables.model.ModelVariable

/**
  * Created by fer on 5/12/16.
  */
case class EF_BayesianNetwork(name: String,
                              dag: DirectedGraph[ModelVariable],
                              distributions: Seq[EF_Distribution]) {

  //val naturalParameters: Vector[DenseVector[Double]] = ???

  //def sufficientStatistics(x: Double): Vector[DenseVector[Double]] = ???

  def toBayesianNetwork: BayesianNetwork = BayesianNetwork(this.name, this.dag, this.distributions.map(_.toDistribution))
}
