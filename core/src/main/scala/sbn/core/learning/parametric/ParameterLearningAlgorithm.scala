package sbn.core.learning.parametric

import breeze.linalg.DenseVector
import sbn.core.data.ImmutableDataSet
import sbn.core.models.EF_BayesianNetwork

/**
  * Created by fer on 5/12/16.
  */
trait ParameterLearningAlgorithm {

  def learn(eF_BayesianNetwork: EF_BayesianNetwork, dataSet: ImmutableDataSet)
           (implicit opDiv: breeze.linalg.operators.OpDiv.Impl2[DenseVector[Double], Int, DenseVector[Double]]): EF_BayesianNetwork

}
