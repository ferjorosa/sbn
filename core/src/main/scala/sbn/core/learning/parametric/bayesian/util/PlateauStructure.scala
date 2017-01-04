package sbn.core.learning.parametric.bayesian.util

import sbn.core.data.ImmutableDataSet
import sbn.core.inference.variational.{Node, VMP}
import sbn.core.models.{BayesianNetwork, CE_BayesianNetwork}
import sbn.core.variables.model.ModelVariable

import scala.collection.mutable.ListBuffer

/**
  * This class helps us describe a dataset composed of N data instances that have been generated from a probability distribution
  * see
  * - http://mlg.eng.cam.ac.uk/zoubin/talks/lect2gm.pdf (pag 6)
  * - https://en.wikipedia.org/wiki/Plate_notation
  *
  */
// data size = nRepetitions (replications)

// vmp quizas se podria cambiar por VariationalInference pensando en el caso de que hubiera varios engines de inferencia
// variacional disponibles (puede ser una variante del vmp por ejemplo)

// Parece que uno de los elementos del Plateau es el DAG (que no la CE_BayesianNetwork) y a partir de el crea la CE_BN

// initialNonReplicatedVariablesList seria aquel grupo de variables que no queremos replicar especificamente (no se su utilidad aun)


case class PlateauStructure(dataSet: ImmutableDataSet,
                            vmp: VMP,
                            seed: Int,
                            bn: BayesianNetwork) {

  val nReplications = dataSet.data.size

  val ce_BayesianNetwork: CE_BayesianNetwork = bn.toCE_BayesianNetwork

  // Las variables que actuen como parametros (en las CE_Distributions) NO entrarian en este grupo
  // por lo que solo utilizamos las variables presentes en el DAG
  // TODO: si permitiemos el ignorar ciertas variables, haria falta un FILTER
  var replicatedVariables: List[ModelVariable] = ce_BayesianNetwork.variables.toList

  // Aquellas que se les haya dicho al plateau que no hay que replicar
  var nonReplicatedVariables: List[ModelVariable] = List.empty

  replicateModel()

  // Each replicated node corresponds to a data instance?
  // Modified in replicateModel()
  var replicatedNodes: List[List[Node]] =
    (for(i <- 0 until nReplications)
      yield
        ce_BayesianNetwork.distributions.map(Node(_)).toList).toList

  // Los nodos creados a partir de las distribuciones de las variables que no se deben de replicar
  var nonReplicatedNodes: List[List[Node]] = List.empty

  private def replicateModel():Unit = {

  }
}

object PlateauStructure {

  def apply(dataSet: ImmutableDataSet, seed: Int, bn: BayesianNetwork): PlateauStructure =
    new PlateauStructure(dataSet, VMP(), seed, bn)
}
