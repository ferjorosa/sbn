package sbn.core.inference

import scala.collection.mutable.ListBuffer

/**
  * Created by fer on 20/12/16.
  */

// One node can represent an observed data instance (that would correspond to a manifest variable) or represent a
// latent node

// Si representa una variable observada se le pueden calcular las sufficient statistics, en caso de que sea latente no.

// Por lo que leo en la clase PlateuStructure, existe un map entre los nodos replicados y la variable a la que se refieren
// es itneresante para el constructor porque puede que existan 2 tipos de nodos (con clase cada uno de ellos y pattern matching)
case class Node() {

  /** Parents of this node */
  val parents: ListBuffer[Node] = ListBuffer.empty
  /** Its children */
  val children: ListBuffer[Node] = ListBuffer.empty
}
