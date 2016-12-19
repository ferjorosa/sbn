package sbn.core.variables

/**
  * Represents a custom generic collection of [[Variable]] objects.
  *
  * @param content the internal collection used to store the variables.
  * @tparam V the type of the stored variables.
  */
case class Variables[V <: Variable](content: Map[String, V]) extends Iterable[V]{

  /**
    * Returns a custom iterator for the collection.
    *
    * @return a custom iterator for the collection.
    */
  override def iterator: Iterator[V] = content.values.iterator

  /**
    * Returns the variable associated to the provided name.
    *
    * @param variableName the provided variable's name.
    * @throws NoSuchElementException if the provided string doesn't correspond to any of the stored variable's name.
    * @return the associated variable.
    */
  def apply(variableName: String): V = content(variableName)
}

/** Factory for the [[Variables]] class. Its main factory method is created by default.*/
object Variables {

  /**
    * Auxiliary factory method. It is used when a collection of variables is provided.
    *
    * @param variables the collection of variables.
    * @tparam V the type of the variables.
    * @return a new [[Variables]] object where the name of the variable is associate to its associated object.
    */
  def apply[V <: Variable](variables: Set[V]): Variables[V] =
    Variables(variables.map(variable =>(variable.name, variable)).toMap)
}
