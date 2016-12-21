package sbn.core.learning.parametric.bayesian
import sbn.core.data.ImmutableDataSet
import sbn.core.learning.parametric.bayesian.util.PlateauStructure
import sbn.core.models.BayesianNetwork

/**
  * Created by fer on 19/12/16.
  */
// Es posible ejecutar el learn varias veces y devolver el mejor resultado (creo que deberia ir en el learn como parametro)
case class VariationalBayes(seed: Int) extends BayesianParameterLearning{

  override def learn(bayesianNetwork: BayesianNetwork, dataSet: ImmutableDataSet): BayesianNetwork = {

    val plateauStructure = PlateauStructure(dataSet, this.seed, bayesianNetwork)

    ???
  }
}
