package ferjorosa.sbn.core.variables

/**
  * Created by fer on 26/10/16.
  */
trait StateSpaceType

case class FiniteStateSpace(numberOfStates: Int, stateNames :List[String]) extends StateSpaceType{
  def this(numberOfStates: Int) = this(numberOfStates, List.empty)
  def this(stateNames :List[String]) = this(stateNames.size, stateNames)

  val mapStatesNames: Map[String, Int] = Map.empty
}

case class RealStateSpace(minInterval: Double, maxInterval: Double) extends StateSpaceType{
  def this() = this(Double.NegativeInfinity, Double.PositiveInfinity)
}




