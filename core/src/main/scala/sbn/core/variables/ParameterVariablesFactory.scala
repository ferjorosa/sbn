package sbn.core.variables

import java.util.UUID

import sbn.core.data.attributes.{Attribute, FiniteStateSpace}

/**
  * Created by fer on 2/12/16.
  */
object ParameterVariablesFactory {

  def newDirichletParameter(name: String, nStates: Int): ParameterVariable  = {
    val attribute = Attribute(name, FiniteStateSpace(nStates))
    ParameterVariable(attribute, new DirichletParameterType, UUID.randomUUID())
  }
}
