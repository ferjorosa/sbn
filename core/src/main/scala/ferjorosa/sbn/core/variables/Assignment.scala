package ferjorosa.sbn.core.variables

/**
  * This class defines an assignment of a value to a variable. The assignment values are encoded as [[Double]] values, therefore
  * In case of finite state space distribution, the [[value]] can be seen as an integer indexing the variable's states.
  *
  * @param variable the selected variable.
  * @param value the variable's assigned value.
  */
case class Assignment(variable: Variable, value: Double) {
  require(variable.isValuePermitted(value), "The assigned value is not permitted")
}

/**
  * This class defines a collection of [[Assignment]] objects and wraps a native collection with some extra checkups and
  * functionality.
  *
  * @param assignments the native collection containing the [[Assignment]] objects.
  * @throws IllegalArgumentException if there are repeated variables in [[assignments]].
  */
@throws[IllegalArgumentException]
case class Assignments(assignments: Set[Assignment]) {
  require(assignments.map(_.variable).size == assignments.size, "There cannot exist repeated variables in the Assignments")
}
