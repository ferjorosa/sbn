package sbn.core.variables
import java.util.UUID

import sbn.core.data.attributes.Attribute
import sbn.core.statistics.exponentialfamily.distributions.EF_UnivariateDistribution

/**
  * Created by fer on 2/12/16.
  */
//TODO: distinguir entre una variable auxiliar de modelo y una variable axuliar por ejemplo de streaming, es decir,
// quizas las parameter variables deberian pertenecer a las model variables, y dentro de ellas ya distinguir entre main y parameter
//todo esto esta relacionado a su vez con el uso de las ef_distributions como base para las ce que se usaran en el aprendizaje bayesiano
trait AuxiliaryVariable extends Variable {

}

