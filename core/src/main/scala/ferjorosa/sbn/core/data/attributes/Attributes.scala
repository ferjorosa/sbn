package ferjorosa.sbn.core.data.attributes

/**
 * An attribute represents a column in the data matrix. It contains the name and type of the elements it contains
 * (discrete, continuous, etc.).
 * @param index the column index of the attribute.
 * @param name the name of the attribute.
 * @param stateSpaceType the [[StateSpaceType]] object that represents its type.
 */
case class Attribute(index: Int, name: String, stateSpaceType: StateSpaceType)

/**
 * Represents a custom collection of [[Attribute]] objects.
 * @constructor Creates an [[Attributes]] object if there are no repeated attribute names.
 * @param attributeList the native collection containing the [[Attribute]] objects.
 * @throws IllegalArgumentException if there are repeated attribute names in the [[attributeList]].
 */
@throws[IllegalArgumentException]
case class Attributes(
                       private val attributeList: List[Attribute]){
  if(attributeList.map(attr => attr.name).distinct.size != attributeList.size)
    throw new IllegalArgumentException("Attribute names cannot be repeated")

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

