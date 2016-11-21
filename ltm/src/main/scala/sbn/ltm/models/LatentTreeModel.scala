package ferjorosa.sbn.ltm.models

import sbn.core.models.graph.UndirectedGraph

/**
 * Created by Fernando on 02/11/2016.
 */
case class LatentTreeModel(name: String, undirectedGraph: UndirectedGraph) {
  require(undirectedGraph.isAcyclic, "A Latent Tree Model's structure cannot contain cycles")

}
