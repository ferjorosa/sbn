package sbn.core.data.attributes

/**
  * Represents a custom immutable collection of [[Attribute]] objects.
  *
  * @param attributeList the native collection containing the [[Attribute]] objects.
  * @param attributeOrder the specific order of the attributes.
  * @throws RuntimeException if there are repeated attribute names in the [[attributeList]]
  *                          or if the [[attributeList]].size != [[attributeOrder]].size
  *                          or if the [[attributeOrder]] contains values out of bound
  *                          or if the [[attributeOrder]] contains repeated values.
  */
case class Attributes (attributeList: List[Attribute], attributeOrder: List[Int]) extends Iterable[Attribute]{
  require(attributeList.map(_.name).distinct.size == attributeList.size, "Attribute names cannot be repeated")
  require(attributeList.size == attributeOrder.size, "The size of the attribute list should be the same of the attribute order collection.")
  require(!attributeOrder.exists( _ >= attributeList.size), "Order value out of bounds.")
  require(attributeOrder.groupBy(identity).collect{
    case (attr, attrRepetitionsList)  if attrRepetitionsList.lengthCompare(1) > 0 => attr
  }.isEmpty, "Repeated values in the attributeOrder collection")

  /** Mapping between each Attribute with its index. */
  val attributeIndexes: Map[Attribute, Int] = attributeList.zipWithIndex.toMap

  /**
    * Returns a custom iterator for the collection.
    *
    * @return a custom iterator for the collection.
    */
  override def iterator: Iterator[Attribute] = orderedAttributeList.iterator

  /**
    * Returns the attributeList ordered by the [[attributeOrder]]. Its order would be different only if a specific order
    * has been provided at the construction.
    *
    * @return the attributeList ordered by the [[attributeOrder]].
    */
  def orderedAttributeList: List[Attribute] = attributeOrder.map(x => attributeList(x))

  /**
    * Returns the requested [[Attribute]] object.
    *
    * @param name the requested attribute's name.
    * @throws RuntimeException if the requested name doesn't exists.
    * @return the requested Attribute.
    */
  def getAttributeByName(name: String): Attribute = {
    this.attributeList.find(attr => attr.name.equals(name))
      .getOrElse(throw new NoSuchElementException("the provided name doesn't coincide with an attribute"))
  }

  /**
   * Returns the attribute associated to the provided index.
    *
   * @param attributeIndex the index of the attribute.
   * @throws RuntimeException if attributeIndex exceeds the bounds of [[attributeList]].
   * @return the attribute associated to the provided index.
   */
  def apply(attributeIndex: Int): Attribute = this.attributeList(this.attributeOrder(attributeIndex))

  /**
    * Return the index associated to the [[Attribute]] object.
    *
    * @param attribute the passed [[Attribute]] object.
    * @throws RuntimeException if the attribute is not present.
    * @return the index of the attribute.
    */
  def indexOf(attribute: Attribute): Int = attributeIndexes(attribute)

}

/** Factory for the [[Attributes]] class. Its main factory method is created by default. */
object Attributes{

  /**
   * Auxiliary factory method. It is used when no special attribute order is provided.
    *
   * @param attributeList the native collection containing the [[Attribute]] objects.
   * @return a new [[Attributes]] object with a default attribute order.
   */
  def apply(attributeList: List[Attribute]): Attributes = {
    val attributeListOrder = attributeList.zipWithIndex.map{case (attr, index) => index}
    Attributes(attributeList, attributeListOrder)
  }
}

