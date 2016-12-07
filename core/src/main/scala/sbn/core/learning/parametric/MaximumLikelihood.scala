package sbn.core.learning.parametric
import breeze.linalg.DenseVector
import sbn.core.data.ImmutableDataSet
import sbn.core.models.EF_BayesianNetwork
import sbn.core.statistics.exponentialfamily.distributions.{EF_ConditionalDistribution, EF_Distribution, EF_UnivariateDistribution}
import sbn.core.variables.{Assignment, Assignments, MainVariable, ModelVariable}


/**
  * Created by fer on 5/12/16.
  */
// In the exponential family, the mle can be seen as a transformation from moment to natural parameters
class MaximumLikelihood extends ParameterLearningAlgorithm{

  def learn(ef_BayesianNetwork: EF_BayesianNetwork, dataSet: ImmutableDataSet): EF_BayesianNetwork = {
    univariateMLE(ef_BayesianNetwork, dataSet)
  }

  private def univariateMLE(ef_BayesianNetwork: EF_BayesianNetwork, dataSet: ImmutableDataSet): EF_BayesianNetwork = {

    val transposed_data = dataSet.transposedDataMatrix

    val orderedDistributionsFromAttributes = dataSet.attributes.map(
      dataAttribute => ef_BayesianNetwork.distributions.find(x => x.variable.attribute equals dataAttribute).get).toVector

    //TODO: Habria que generalizar este proceso para las distribuciones condicionadas
    val sumSuffStatistics: IndexedSeq[DenseVector[Double]] =
    for(i <- transposed_data.indices)
      yield transposed_data(i).map(value => orderedDistributionsFromAttributes(i) match {
        case univariate: EF_UnivariateDistribution => univariate.sufficientStatistics(value)
      }).reduce(_ + _)

    // Elementwise divison
    val normalizedSumSuffStatistics: IndexedSeq[DenseVector[Double]] = sumSuffStatistics.map(_ :/ dataSet.data.size.asInstanceOf[Double])

    val momentParameters = normalizedSumSuffStatistics

    val learnedDistributions: Seq[EF_Distribution] = ef_BayesianNetwork.distributions.map(dist => {
      if(orderedDistributionsFromAttributes.contains(dist)) {
        val distIndex = orderedDistributionsFromAttributes.indexOf(dist)
        dist match {
          case univariate: EF_UnivariateDistribution => univariate.update(momentParameters(distIndex))
        }
      }
      else
        dist
    })

    EF_BayesianNetwork(ef_BayesianNetwork.name, ef_BayesianNetwork.dag, learnedDistributions)
  }

  private def compoundMLE(ef_BayesianNetwork: EF_BayesianNetwork, dataSet: ImmutableDataSet): EF_BayesianNetwork = {

    //observed distributions (the only ones that can be learned with MLE)
    val orderedDistributionsFromAttributes = dataSet.attributes.map(
      dataAttribute => ef_BayesianNetwork.distributions.find(x => x.variable.attribute equals dataAttribute).get).toVector

    // Una vez tenemos las distribuciones observadas iteramos las data instancias

    for(instance <- dataSet.data){

      val pairsVarUniSs = orderedDistributionsFromAttributes
        .filter(_.isInstanceOf[EF_UnivariateDistribution])
        .map(dist => (
          dist.variable,
          dist.asInstanceOf[EF_UnivariateDistribution].sufficientStatistics(instance.attributes.indexOf(dist.variable.attribute)))
        )

      val pairVarCondSs = orderedDistributionsFromAttributes
        .filter(_.isInstanceOf[EF_ConditionalDistribution])
        .map(dist => {
          val pairParentSs = pairsVarUniSs.filter(x => dist.asInstanceOf[EF_ConditionalDistribution].parents.contains(x._1.asInstanceOf[MainVariable]))
          val assignments = MaximumLikelihood.generateAssignments(pairParentSs)
          dist.asInstanceOf[EF_ConditionalDistribution].sufficientStatistics(assignments,instance.attributes.indexOf(dist.variable.attribute))
          }
          )
    }
    null
  }
}

object MaximumLikelihood {

  // Realmente esta mierda solo vale para las distribuciones con padres multinomiales y como sus SS son un vector de todo 0s y un 1
  // pues es facil de programar
  private def generateAssignments(pairsParentSS: Iterable[(ModelVariable, DenseVector[Double])]): Assignments = {
    Assignments(pairsParentSS.map{case (a, b) => Assignment(a, b.data.indexOf(1))}.toSet)
  }
}
