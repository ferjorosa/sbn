package sbn.core.learning.parametric
import breeze.linalg.{DenseVector, sum}
import sbn.core.data.{DataInstance, ImmutableDataSet}
import sbn.core.models.EF_BayesianNetwork
import sbn.core.statistics.distributions.exponentialfamily.{EF_ConditionalDistribution, EF_Distribution, EF_UnivariateDistribution}
import sbn.core.variables.model.ModelVariable
import sbn.core.variables.{Assignment, Assignments}

import scala.collection.parallel.immutable.ParVector

/**
  * Created by fer on 5/12/16.
  */
// In the exponential family, the mle can be seen as a transformation from moment to natural parameters
class MaximumLikelihood extends ParameterLearningAlgorithm{

  def learn(ef_BayesianNetwork: EF_BayesianNetwork, dataSet: ImmutableDataSet): EF_BayesianNetwork = {
    //univariateMLE(ef_BayesianNetwork, dataSet)
    null
  }

  def MLE(ef_BayesianNetwork: EF_BayesianNetwork, dataSet: ImmutableDataSet): EF_BayesianNetwork = {

    //Aquellas distribuciones sobre un atributo presente en los datos
    val observedDistributions = dataSet.attributes.map(
      dataAttribute => ef_BayesianNetwork.distributions.find(x => x.variable.attribute equals dataAttribute).get).toVector

    val distSumSS: Vector[Map[Assignments, DenseVector[Double]]] = dataSet.data.map{ instance =>
      observedDistributions.map{
        case univariate: EF_UnivariateDistribution =>
          (Assignments(Set.empty[Assignment]), univariate.sufficientStatistics(instance.value(univariate.variable.attribute)))
        case condicional: EF_ConditionalDistribution =>
          val assignments = MaximumLikelihood.generateAssignments(condicional.parents.toSet, instance)
          (assignments, condicional.sufficientStatistics(assignments, instance.value(condicional.variable.attribute)))
      }
    }.transpose.map(_.groupBy(_._1)
      .map{case (k, v) => k -> v.reduceLeft{(a, b) => (a._1, a._2 + b._2)}._2})

    val normalizedSumSS: Vector[Map[Assignments, DenseVector[Double]]] =
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

    EF_BayesianNetwork(ef_BayesianNetwork.name, ef_BayesianNetwork.dag, learnedDistributions.toVector)
  }

  def parallelMLE(ef_BayesianNetwork: EF_BayesianNetwork, dataSet: ImmutableDataSet) = {

    //Aquellas distribuciones sobre un atributo presente en los datos
    val observedDistributions = dataSet.attributes.map(
      dataAttribute => ef_BayesianNetwork.distributions.find(x => x.variable.attribute equals dataAttribute).get).toVector

    val distSumSS: ParVector[Map[Assignments, DenseVector[Double]]] = dataSet.data.map{ instance =>
      observedDistributions.map{
        case univariate: EF_UnivariateDistribution =>
          (Assignments(Set.empty[Assignment]), univariate.sufficientStatistics(instance.value(univariate.variable.attribute)))
        case condicional: EF_ConditionalDistribution =>
          val assignments = MaximumLikelihood.generateAssignments(condicional.parents.toSet, instance)
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

    EF_BayesianNetwork(ef_BayesianNetwork.name, ef_BayesianNetwork.dag, learnedDistributions.toVector)

  }

  def alternativeMLE(ef_BayesianNetwork: EF_BayesianNetwork, dataSet: ImmutableDataSet): EF_BayesianNetwork = {

    //Aquellas distribuciones sobre un atributo presente en los datos
    val observedDistributions = dataSet.attributes.map(
      dataAttribute => ef_BayesianNetwork.distributions.find(x => x.variable.attribute equals dataAttribute).get).toVector

    val distSumSS = dataSet.data.map{ instance =>
      observedDistributions.map{
        case univariate: EF_UnivariateDistribution =>
          (Assignments(Set.empty[Assignment]), univariate.sufficientStatistics(instance.value(univariate.variable.attribute)))
        case condicional: EF_ConditionalDistribution =>
          val assignments = MaximumLikelihood.generateAssignments(condicional.parents.toSet, instance)
          (assignments, condicional.sufficientStatistics(assignments, instance.value(condicional.variable.attribute)))
      }
    }.foldLeft(observedDistributions.map(_.generalZeroSufficientStatistics)) {
      (x: Seq[Map[Assignments, DenseVector[Double]]], y: Seq[(Assignments, DenseVector[Double])]) => {
        for (i <- x.indices) yield {
          x(i) + (y(i)._1 -> (x(i)(y(i)._1) + y(i)._2))
        }
      }.toVector
    }

    val normalizedSumSS =
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

    val newBn = EF_BayesianNetwork(ef_BayesianNetwork.name, ef_BayesianNetwork.dag, learnedDistributions.toVector)

    newBn
  }

  def alternativeParallelMLE(ef_BayesianNetwork: EF_BayesianNetwork, dataSet: ImmutableDataSet): EF_BayesianNetwork = {
    //Aquellas distribuciones sobre un atributo presente en los datos
    val observedDistributions = dataSet.attributes.map(
      dataAttribute => ef_BayesianNetwork.distributions.find(x => x.variable.attribute equals dataAttribute).get).toVector

    val distSumSS = dataSet.data.par.map{ instance =>
      observedDistributions.map{
        case univariate: EF_UnivariateDistribution =>
          (Assignments(Set.empty[Assignment]), univariate.sufficientStatistics(instance.value(univariate.variable.attribute)))
        case condicional: EF_ConditionalDistribution =>
          val assignments = MaximumLikelihood.generateAssignments(condicional.parents.toSet, instance)
          (assignments, condicional.sufficientStatistics(assignments, instance.value(condicional.variable.attribute)))
      }
    }.foldLeft(observedDistributions.map(_.generalZeroSufficientStatistics)) {
      (x: Seq[Map[Assignments, DenseVector[Double]]], y: Seq[(Assignments, DenseVector[Double])]) => {
        for (i <- x.indices) yield {
          x(i) + (y(i)._1 -> (x(i)(y(i)._1) + y(i)._2))
        }
      }.toVector
    }

    val normalizedSumSS =
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

    val newBn = EF_BayesianNetwork(ef_BayesianNetwork.name, ef_BayesianNetwork.dag, learnedDistributions.toVector)

    newBn
  }

  private def groupSS(collection: Seq[Seq[(Assignments, DenseVector[Double])]], observedDistributions: Seq[EF_Distribution]): Seq[Map[Assignments, DenseVector[Double]]] = {

    //val hash: Seq[HashMap[Assignments, DenseVector[Double]]] = observedDistributions.map(x => HashMap.

    collection.foldLeft(observedDistributions.map(_.generalZeroSufficientStatistics)){
      (x: Seq[Map[Assignments, DenseVector[Double]]], y: Seq[(Assignments, DenseVector[Double])]) => {
        for(i <- x.indices) yield {
          val mapperino = x(i)
          val assignmentKey = y(i)._1
          val assignmentValue = y(i)._2
          mapperino + (assignmentKey -> (mapperino(assignmentKey) + assignmentValue))
        }
      }
    }
  }

}

object MaximumLikelihood {

  // Realmente esta mierda solo vale para las distribuciones con padres multinomiales y como sus SS son un vector de todo 0s y un 1
  // pues es facil de programar
  private def generateAssignments(pairsParentSS: Iterable[(ModelVariable, DenseVector[Double])]): Assignments = {
    Assignments(pairsParentSS.map{case (a, b) => Assignment(a, b.data.indexOf(1))}.toSet)
  }

  // Esto se podria optimizar con atributos(un Map mutable o algo asi, bueno mutable seguramente no valdria si quiero paralelizar easy)
  private def generateAssignments(parents: Set[ModelVariable], instance: DataInstance): Assignments =
    Assignments(parents.map(x => Assignment(x, instance.value(x.attribute))))

  //implicit val SemigroupDenseVector: Semigroup[DenseVector[Double]] = Semigroup.instance((a, b) => a + b)
}
