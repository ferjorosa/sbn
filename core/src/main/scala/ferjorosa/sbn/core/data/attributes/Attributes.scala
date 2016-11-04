package ferjorosa.sbn.core.data.attributes

/**
  * Represents a custom collection of [[Attribute]] objects.
  * @param attributeList the native collection containing the [[Attribute]] objects.
  * @param attributeOrder the specific order of the attributes.
  * @throws IllegalArgumentException if there are repeated attribute names in the [[attributeList]]
  *                                  or if the [[attributeOrder]] contains values out of bound.
  */
@throws[IllegalArgumentException]
case class Attributes (attributeList: List[Attribute], attributeOrder: Set[Int]){
  if(attributeList.map(attr => attr.name).distinct.size != attributeList.size)
    throw new IllegalArgumentException("Attribute names cannot be repeated")
  if (attributeOrder.exists(x => x >= attributeList.size))
    throw new IllegalArgumentException("Order value out of bounds")

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
  def size: Int = this.attributeList.size

  /**
   * Returns the attribute associated to the provided index.
   * @param attributeIndex the index of the attribute.
   * @throws IndexOutOfBoundsException if attributeIndex exceeds the bounds of [[attributeList]].
   * @return the attribute associated to the provided index.
   */
  @throws[IndexOutOfBoundsException]
  def apply(attributeIndex: Int): Attribute = this.attributeList(attributeIndex)

}

/** Factory for the [[Attributes]] class. Its main factory method is created by default. */
object Attributes{

  /**
   * Auxiliary factory method. It is used when no special attribute order is provided.
   * @param attributeList the native collection containing the [[Attribute]] objects.
   * @return a new [[Attributes]] object with a default attribute order.
   */
  def apply(attributeList: List[Attribute]): Attributes = {
    val attributeListOrder = attributeList.zipWithIndex.map{case (attr, index) => index}.toSet
    Attributes(attributeList, attributeListOrder)
  }
}

