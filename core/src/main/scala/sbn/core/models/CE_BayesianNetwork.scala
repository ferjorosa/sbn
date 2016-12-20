package sbn.core.models

import sbn.core.models.graph.DirectedGraph
import sbn.core.statistics.distributions.learning.CE_Distribution
import sbn.core.variables.model.ModelVariable

/**
  * Created by fer on 2/12/16.
  */
case class CE_BayesianNetwork(name: String, dag: DirectedGraph[ModelVariable], distributions: Seq[CE_Distribution]){


}
