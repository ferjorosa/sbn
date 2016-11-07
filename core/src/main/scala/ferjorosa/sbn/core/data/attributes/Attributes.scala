package ferjorosa.sbn.core.data.attributes

/**
  * Represents a custom immutable collection of [[Attribute]] objects.
  * @param attributeList the native collection containing the [[Attribute]] objects.
  * @param attributeOrder the specific order of the attributes.
  * @throws IllegalArgumentException if there are repeated attribute names in the [[attributeList]]
  *                                  or if the [[attributeOrder]] contains values out of bound
  *                                  or if the [[attributeOrder]] contains repeated values.
  */
@throws[IllegalArgumentException]
case class Attributes (attributeList: List[Attribute], attributeOrder: List[Int]) extends Iterable[Attribute]{
  if(attributeList.map(_.name).distinct.size != attributeList.size)
    throw new IllegalArgumentException("Attribute names cannot be repeated")
  if(attributeList.size != attributeOrder.size)
    throw new IllegalArgumentException("The size of the attribute list should be the same of the attribute order collection")
  if (attributeOrder.exists( _ >= attributeList.size))
    throw new IllegalArgumentException("Order value out of bounds")
  if(attributeOrder.groupBy(identity).collect{case (attr, repetitionList) if repetitionList.lengthCompare(1) > 0 => attr}.nonEmpty)
    throw new IllegalArgumentException("Repeated values in the attributeOrder collection")

  /**
    * Returns the attributeList ordered by the [[attributeOrder]].
    * @return the attributeList ordered by the [[attributeOrder]].
    */
  def orderedAttributeList: List[Attribute] = attributeOrder.map(x => attributeList(x))

  /**
   * Returns an Option containing the requested Attribute or 'None'.
   * @param name the requested attribute's name.
   * @return an Option containing the requested Attribute or 'None'.
   */
  def getAttributeByName(name: String): Option[Attribute] ={
    this.attributeList.find(attr => attr.name.equals(name))
  }

  /**
   * Returns the size of the [[attributeList]].
   * @return the size of the [[attributeList]].
   */
  override def size: Int = this.attributeList.size

  /**
    * Returns a custom iterator for the collection.
    * @return a custom iterator for the collection.
    */
  override def iterator: Iterator[Attribute] = orderedAttributeList.iterator

  /**
   * Returns the attribute associated to the provided index.
   * @param attributeIndex the index of the attribute.
   * @throws IndexOutOfBoundsException if attributeIndex exceeds the bounds of [[attributeList]].
   * @return the attribute associated to the provided index.
   */
  @throws[IndexOutOfBoundsException]
  def apply(attributeIndex: Int): Attribute = this.attributeList(this.attributeOrder(attributeIndex))

}

/** Factory for the [[Attributes]] class. Its main factory method is created by default. */
object Attributes{

  /**
   * Auxiliary factory method. It is used when no special attribute order is provided.
   * @param attributeList the native collection containing the [[Attribute]] objects.
   * @return a new [[Attributes]] object with a default attribute order.
   */
  def apply(attributeList: List[Attribute]): Attributes = {
    val attributeListOrder = attributeList.zipWithIndex.map{case (attr, index) => index}
    Attributes(attributeList, attributeListOrder)
  }
}

