package ferjorosa.sbn.core.variables


/**
  * @constructor
  * @param numberOfStates
  * @param stateNames
  * @param mapStatesNames
  */
case class FiniteStateSpace(numberOfStates: Int, stateNames :List[String], mapStatesNames: Map[String, Int]) extends StateSpaceType{
  def getIndexOfState (stateName : String): Int = this.mapStatesNames(stateName)
}

object FiniteStateSpace{

  /**
    * Auxiliary constructor
    * @param numberOfStates the number of states of the attribute
    * @return a new FiniteStateSpace object
    */
  def apply(numberOfStates: Int): FiniteStateSpace ={
    var _stateNames: List[String] = List.empty
    var _mapStatesNames: Map[String, Int] = Map.empty

    for(i<-0 until numberOfStates){
      _stateNames = _stateNames.::("s" + i)
      _mapStatesNames = _mapStatesNames + ("s"+i -> i)
    }

    new FiniteStateSpace(numberOfStates, _stateNames, _mapStatesNames)
  }

  /**
    * Auxiliary constructor
    * @param stateNames the state names of the attribute
    * @return a new finiteStateSpace object
    */
  def apply(stateNames :List[String]) = {
    var _mapStatesNames: Map[String, Int] = Map.empty

    for(i <- stateNames.indices)
      _mapStatesNames = _mapStatesNames + ("s"+i -> i)

    new FiniteStateSpace(stateNames.size, stateNames, _mapStatesNames)
  }
}

/**
  *
  * @param minInterval
  * @param maxInterval
  */
case class RealStateSpace(minInterval: Double, maxInterval: Double) extends StateSpaceType{
  def this() = this(Double.NegativeInfinity, Double.PositiveInfinity)
}

/**
  *
  * @param numberOfStates
  */
case class SparseFiniteStateSpace(numberOfStates: Int) extends StateSpaceType

sealed trait StateSpaceType
