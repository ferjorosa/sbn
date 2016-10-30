package ferjorosa.sbn.core.data

import ferjorosa.sbn.core.variables.StateSpaceType

/**
  * Created by fer on 26/10/16.
  */
case class Attribute(index: Int, name: String, stateSpaceType: StateSpaceType)

case class Attributes(attributeList: List[Attribute]){

  if(attributeList.map(attr => attr.name).distinct.size != attributeList.size)
    throw new IllegalArgumentException("Attribute names cannot be repeated")

  def getAttributeByName(name: String): Option[Attribute] ={
    this.attributeList.find(attr => attr.name.equals(name))
  }

  def size: Int = this.attributeList.size

  def apply(attributeIndex: Int): Attribute = this.attributeList(attributeIndex)
}

