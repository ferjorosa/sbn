package sbn.core.learning.parametric.bayesian

import sbn.core.data.ImmutableDataSet
import sbn.core.models.BayesianNetwork

/**
  * Created by fer on 19/12/16.
  */
trait BayesianParameterLearning{

  def learn(bayesianNetwork: BayesianNetwork, dataSet: ImmutableDataSet): BayesianNetwork

}
