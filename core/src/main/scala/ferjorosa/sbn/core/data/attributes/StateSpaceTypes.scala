package ferjorosa.sbn.core.data.attributes

//TODO: Properly create the SparseStateSpace class and seal the StateSpaceType trait.

/**
 * Base trait for the state-space types.
 */
trait StateSpaceType

/**
 * This class defines the finite state-space. The state-space for discrete attributes & variables.
 * @param numberOfStates the number of states.
 * @param stateNames the collection of state names.
 * @param mapStatesNames the map between the index of the state and its name.
 */
case class FiniteStateSpace(numberOfStates: Int, stateNames :List[String], mapStatesNames: Map[String, Int]) extends StateSpaceType{

  /**
   * Returns the index associated to the state name.
   * @param stateName the provided state name.
   * @throws NoSuchElementException if the state name doesn't correspond to an index.
   * @return the index associated to the state name.
   */
  @throws[NoSuchElementException]
  def getIndexOfState (stateName : String): Int = this.mapStatesNames(stateName)
}

/** Auxiliary factory for the [[FiniteStateSpace]] class. */
object FiniteStateSpace{

  /**
    * Auxiliary constructor.
    * @param numberOfStates the number of states of the attribute.
    * @return a new FiniteStateSpace instance.
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
    * Auxiliary constructor.
    * @param stateNames the state names of the attribute.
    * @return a new finiteStateSpace instance.
    */
  def apply(stateNames :List[String]) = {
    var _mapStatesNames: Map[String, Int] = Map.empty

    for(i <- stateNames.indices)
      _mapStatesNames = _mapStatesNames + (stateNames(i) -> i)

    new FiniteStateSpace(stateNames.size, stateNames, _mapStatesNames)
  }
}

/**
  * This class defines the real state-space. The state-space for continuous attributes & variables.
  * @param minInterval the minimum value of the interval.
  * @param maxInterval the maximum value of the interval.
  */
case class RealStateSpace(minInterval: Double, maxInterval: Double) extends StateSpaceType{

  /**
   * Parameterless auxiliary constructor.
   * @return a real-state space object whose intervals are infinite.
   */
  def this() = this(Double.NegativeInfinity, Double.PositiveInfinity)
}
