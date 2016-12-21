package sbn.core.statistics.distributions.learning

import sbn.core.statistics.distributions.Distribution
import sbn.core.variables.model.ModelVariable

/**
  * Created by fer on 2/12/16.
  */
trait CE_Distribution {

  val variable: ModelVariable

  def toDistribution: Distribution
}
