package sbn.core.learning.parametric.bayesian
import sbn.core.data.ImmutableDataSet
import sbn.core.inference.variational.VariationalInference
import sbn.core.learning.parametric.bayesian.util.PlateauStructure
import sbn.core.models.BayesianNetwork
import sbn.core.models.graph.DirectedGraph
import sbn.core.statistics.distributions.Distribution
import sbn.core.variables.Variable
import sbn.core.variables.model.ModelVariable

/**
  * Created by fer on 19/12/16.
  */
// Mas adelante quizas cambie "seed" a parametro del learn, pero por ahora esta bien
// TODO: Creo que no es necesario SEED si usamos "ThreadLocalRandom"
case class VariationalBayes(seed: Int, variationalInferenceMethod: VariationalInference) extends BayesianParameterLearning{


  override def learn(bayesianNetwork: BayesianNetwork, dataSet: ImmutableDataSet): BayesianNetwork = {

    val plateauStructure = PlateauStructure(dataSet, this.seed, bayesianNetwork)

    ???
  }

  def learnNew(dag: DirectedGraph[ModelVariable],
               dataSet: ImmutableDataSet,
               priorDistributions: Map[ModelVariable, Distribution]): BayesianNetwork = {

    // 1- Definir todas las distribuciones a priori (realmente solo nos interesan para las LVs)
    val priorBN = BayesianNetwork.initializeWithPriors(dag, priorDistributions)

    // 2- Definir la estructura de nodos inicial (inicializar la PlateStructure)


    ???
  }
}
