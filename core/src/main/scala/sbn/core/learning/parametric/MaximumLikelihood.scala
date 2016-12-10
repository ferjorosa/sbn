package sbn.core.learning.parametric
import breeze.linalg.{sum, DenseVector}
import sbn.core.data.{DataInstance, ImmutableDataSet}
import sbn.core.models.EF_BayesianNetwork
import sbn.core.statistics.exponentialfamily.distributions.{EF_ConditionalDistribution, EF_Distribution, EF_UnivariateDistribution}
import sbn.core.variables.{Assignment, Assignments, MainVariable, ModelVariable}

import scala.collection.parallel.immutable.ParVector

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

  def compoundMLE2(ef_BayesianNetwork: EF_BayesianNetwork, dataSet: ImmutableDataSet): EF_BayesianNetwork = {

    //Aquellas distribuciones sobre un atributo presente en los datos
    val observedDistributions = dataSet.attributes.map(
      dataAttribute => ef_BayesianNetwork.distributions.find(x => x.variable.attribute equals dataAttribute).get).toVector

    val distSumSS: ParVector[Map[Assignments, DenseVector[Double]]] = dataSet.data.map{ instance =>
      observedDistributions.map{
        case univariate: EF_UnivariateDistribution =>
          (Assignments(Set.empty[Assignment]), univariate.sufficientStatistics(instance.value(univariate.variable.attribute)))
        case condicional: EF_ConditionalDistribution =>
          val assignments = MaximumLikelihood.generateAssignments(condicional.parents, instance)
          (assignments, condicional.sufficientStatistics(assignments, instance.value(condicional.variable.attribute)))
      }
    }.transpose.par.map(_.groupBy(_._1)
      .map{case (k, v) => k -> v.reduceLeft{(a, b) => (a._1, a._2 + b._2)}._2})

    val normalizedSumSS: ParVector[Map[Assignments, DenseVector[Double]]] =
      for(sumSS <- distSumSS) yield sumSS.mapValues(x => x :/ sum(x))

    val learnedDistributions: Seq[EF_Distribution] = ef_BayesianNetwork.distributions.map(dist => {
      if (observedDistributions.contains(dist)) {
        val distIndex = observedDistributions.indexOf(dist)
        dist match {
          case univariate: EF_UnivariateDistribution =>
            val momentParameters = normalizedSumSS(distIndex).get(Assignments(Set.empty[Assignment])).get
            univariate.update(momentParameters)

          case condicional: EF_ConditionalDistribution =>
            val momentParameters = normalizedSumSS(distIndex)
            condicional.update(momentParameters)
        }
      }
      else
        dist
    })

    EF_BayesianNetwork(ef_BayesianNetwork.name, ef_BayesianNetwork.dag, learnedDistributions)
  }
}

object MaximumLikelihood {

  // Realmente esta mierda solo vale para las distribuciones con padres multinomiales y como sus SS son un vector de todo 0s y un 1
  // pues es facil de programar
  private def generateAssignments(pairsParentSS: Iterable[(ModelVariable, DenseVector[Double])]): Assignments = {
    Assignments(pairsParentSS.map{case (a, b) => Assignment(a, b.data.indexOf(1))}.toSet)
  }

  // Esto se podria optimizar con atributos(un Map mutable o algo asi, bueno mutable seguramente no valdria si quiero paralelizar easy)
  private def generateAssignments(parents: Set[MainVariable], instance: DataInstance): Assignments =
    Assignments(parents.map(x => Assignment(x, instance.value(x.attribute))))

  //implicit val SemigroupDenseVector: Semigroup[DenseVector[Double]] = Semigroup.instance((a, b) => a + b)
}
