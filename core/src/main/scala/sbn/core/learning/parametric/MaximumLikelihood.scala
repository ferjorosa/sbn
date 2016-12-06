package sbn.core.learning.parametric
import breeze.linalg.DenseVector
import sbn.core.data.ImmutableDataSet
import sbn.core.models.EF_BayesianNetwork
import sbn.core.statistics.exponentialfamily.distributions.{EF_Distribution, EF_UnivariateDistribution}


/**
  * Created by fer on 5/12/16.
  */
// In the exponential family, the mle can be seen as a transformation from moment to natural parameters
class MaximumLikelihood extends ParameterLearningAlgorithm{

  def learn(eF_BayesianNetwork: EF_BayesianNetwork, dataSet: ImmutableDataSet): EF_BayesianNetwork = {

    val transposed_data = dataSet.transposedDataMatrix

    val orderedDistributionsFromAttributes = dataSet.attributes.map(
      dataAttribute => eF_BayesianNetwork.distributions.find(x => x.variable.attribute equals dataAttribute).get).toVector

    val sumSuffStatistics: IndexedSeq[DenseVector[Double]] =
      for(i <- transposed_data.indices)
        yield transposed_data(i).map(value => orderedDistributionsFromAttributes(i) match {
          case univariate: EF_UnivariateDistribution => univariate.sufficientStatistics(value)
        }).reduce(_ + _)

    // Elementwise divison
    val normalizedSumSuffStatistics: IndexedSeq[DenseVector[Double]] = sumSuffStatistics.map(_ :/ dataSet.data.size.asInstanceOf[Double])

    val momentParameters = normalizedSumSuffStatistics

    val learnedDistributions: Seq[EF_Distribution] = eF_BayesianNetwork.distributions.map(dist => {
      if(orderedDistributionsFromAttributes.contains(dist)) {
        val distIndex = orderedDistributionsFromAttributes.indexOf(dist)
        dist match {
          case univariate: EF_UnivariateDistribution => univariate.update(momentParameters(distIndex))
        }
      }
      else
        dist
    })

    EF_BayesianNetwork(eF_BayesianNetwork.name, eF_BayesianNetwork.dag, learnedDistributions)
  }
}
