package sbn.core.data.attributes

//TODO: Properly create the SparseStateSpace class and seal the StateSpaceType trait.

/**
 * Base trait for the state-space types.
 */
trait StateSpaceType{

  /**
    * Checks if the provided value belongs to the state space or not.
    *
    * @param value the provided value.
    * @return a [[Boolean]] value representing the answer.
    */
  def isValuePermitted(value: Double): Boolean
}

/**
  * This class defines the finite state-space. The state-space for discrete attributes & variables.
  *
  * @param numberOfStates the number of states.
  * @param stateNames the collection of state names.
  * @param mapStatesNames the map between the index of the state and its name.
  * @throws RuntimeException if [[numberOfStates]] <= 0
  *                          or if [[stateNames]].size != [[numberOfStates]]
  *                          or if [[mapStatesNames]].keys.size != [[numberOfStates]]
  *                          or if there are repeated [[mapStatesNames]].keys
  *                          or if there are repeated [[mapStatesNames]].values.
  */
case class FiniteStateSpace (numberOfStates: Int, stateNames: Vector[String], mapStatesNames: Map[String, Int]) extends StateSpaceType{
  require(numberOfStates > 0, "Number of states > 0")
  require(numberOfStates == stateNames.size, "The stateNames collection must have the same size as the number of states")
  require(mapStatesNames.keys.size == numberOfStates, "Mapping collection size and the numberOfStates doesn't coincide")
  require(mapStatesNames.values.toSet.size == mapStatesNames.values.size, "Repeated mapStatesNames values")

  /** @inheritdoc */
  override def isValuePermitted(value: Double): Boolean = value.asInstanceOf[Int] < this.numberOfStates

  /**
    * Returns the index associated to the state name.
    *
    * @param stateName the provided state name.
    * @throws RuntimeException if the state name doesn't correspond to an index.
    * @return the index associated to the state name.
    */
  def getIndexOfState (stateName : String): Int = this.mapStatesNames(stateName)

  /**
    * Returns a collection of the state indexes, same size as the [[stateNames]].
    *
    * @return a collection of the state indexes, same size as the [[stateNames]].
    */
  def stateIndexes: Vector[Int] = (for( i<-0 until numberOfStates) yield i).toVector
}

/** Factory for the [[FiniteStateSpace]] class. Its main factory method is created by default. */
object FiniteStateSpace{

  /**
    * Auxiliary factory method. It will generate the state names the following way (s0, s1, s2, etc)
    * and the mappings between the state names and their indexes.
    *
    * @param numberOfStates the number of states of the attribute.
    * @return a new FiniteStateSpace instance.
    */
  def apply(numberOfStates: Int): FiniteStateSpace ={
    var _stateNames: Vector[String] = Vector.empty
    var _mapStatesNames: Map[String, Int] = Map.empty

    for(i<-0 until numberOfStates){
      _stateNames = _stateNames.:+("s" + i)
      _mapStatesNames = _mapStatesNames + ("s"+i -> i)
    }

    FiniteStateSpace(numberOfStates, _stateNames, _mapStatesNames)
  }

  /**
    * Auxiliary factory method. It will generate the mappings between the state names and their indexes.
    *
    * @param stateNames the state names of the attribute.
    * @return a new finiteStateSpace instance.
    */
  def apply(stateNames :Vector[String]): FiniteStateSpace = {
    var _mapStatesNames: Map[String, Int] = Map.empty

    for(i <- stateNames.indices)
      _mapStatesNames = _mapStatesNames + (stateNames(i) -> i)

    FiniteStateSpace(stateNames.size, stateNames, _mapStatesNames)
  }
}

/**
  * This class defines the real state-space. The state-space for continuous attributes & variables.
  *
  * @param minInterval the minimum value of the interval.
  * @param maxInterval the maximum value of the interval.
  */
case class RealStateSpace (minInterval: Double, maxInterval: Double) extends StateSpaceType {

  /** @inheritdoc */
  override def isValuePermitted(value: Double): Boolean = value > maxInterval || value < minInterval
}

/** Factory for the [[RealStateSpace]] class. Its main factory method is created by default. */
object RealStateSpace{

  /**
    * Parameterless auxiliary factory method. It will generate a real-state space object whose intervals are infinite.
    *
    * @return a real-state space object whose intervals are infinite.
    */
  def apply(): RealStateSpace = RealStateSpace(Double.NegativeInfinity, Double.PositiveInfinity)
}
