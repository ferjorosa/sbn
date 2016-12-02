package sbn.core.variables

import java.util.UUID

import sbn.core.data.attributes._

/**
  * This trait defines an interface for both manifest and latent variables.
  */
trait Variable extends Product with Serializable{

  /**
    * The variable's ID.
    *
    * @return the variable's ID.
    */
  def id: UUID

  /**
    * The [[Attribute]] it was created from.
    *
    * @return the attribute it was created from.
    */
  def attribute: Attribute

  /**
    * The variable's name.
    *
    * @return the variable's name.
    */
  def name: String = this.attribute.name

  /**
    * Returns a [[Boolean]] indicating if the passed value is belongs to the variable's domain (its state space limits).
    *
    * @param value the passed value.
    * @return a [[Boolean]] value indicating if the value belongs to the variable's domain and therefore is permitted.
    */
  def isValuePermitted(value: Double): Boolean = attribute.isValuePermitted(value)

}