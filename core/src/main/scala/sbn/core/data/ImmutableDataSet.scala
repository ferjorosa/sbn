package sbn.core.data

import sbn.core.data.attributes.{Attribute, Attributes}

/**
  * The class represents a collection of [[DataInstance]]] objects that represents its data.
  *
  * @param name the name of the DataSet.
  * @param attributes the [[Attributes]] object representing the data instance's columns.
  * @param data the collection of [[DataInstance]] objects that represent its data.
  */
case class ImmutableDataSet(name: String, attributes: Attributes, data: Vector[DataInstance]){

  val dataMatrix: Vector[Vector[Double]] = data.map(_.values)

  val transposedDataMatrix: Vector[Vector[Double]] = dataMatrix.transpose
}
