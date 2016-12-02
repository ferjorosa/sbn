package sbn.core.variables
import java.util.UUID

import sbn.core.data.attributes.Attribute

/**
  * Created by fer on 2/12/16.
  */
trait AuxiliaryVariable extends Variable{

  /**
    * The parameterVariable's distribution type (DirichletParameter, GammaParameter, GaussianParameter, etc.).
    *
    * @return the parameterVariable's distribution type.
    */
  def parameterDistributionType: ParameterDistributionType
}

case class ParameterVariable(attribute: Attribute,
                             parameterDistributionType: ParameterDistributionType,
                             id: UUID) extends AuxiliaryVariable {

  require(parameterDistributionType.isAttributeCompatible(attribute), "Attribute is not compatible: "+ parameterDistributionType + " & " + attribute.stateSpaceType)
}