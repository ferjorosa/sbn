package sbn.core.inference

import sbn.core.statistics.distributions.exponentialfamily.EF_UnivariateDistribution
import sbn.core.statistics.distributions.learning.CE_Distribution

import scala.collection.mutable.ListBuffer

/**
  * Created by fer on 20/12/16.
  */

// One node can represent an observed data instance (that would correspond to a manifest variable) or represent a
// latent node

// Si representa una variable observada se le pueden calcular las sufficient statistics, en caso de que sea latente no.

// Por lo que leo en la clase PlateuStructure, existe un map entre los nodos replicados y la variable a la que se refieren
// es itneresante para el constructor porque puede que existan 2 tipos de nodos (con clase cada uno de ellos y pattern matching)
case class Node(ce_distribution: CE_Distribution) {

  // TODO: Es importante que todos los nodos posean las mismas prioris (creo) porque si utilizamos valores aleatorios
  // entonces generaremos resultados diferentes en cada caso.
  val QDist: EF_UnivariateDistribution = ce_distribution.variable.newEFUnivariateDistribution

  val PDist: CE_Distribution = ce_distribution

  /** Parents of this node */
    // Aqui depende de si la distribucion es condicional o marginal
  val parents: ListBuffer[Node] = ListBuffer.empty
  /** Its children */
    // Aqui depende de si tiene variables
  val children: ListBuffer[Node] = ListBuffer.empty
}
