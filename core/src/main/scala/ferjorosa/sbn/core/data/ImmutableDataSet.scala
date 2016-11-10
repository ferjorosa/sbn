package ferjorosa.sbn.core.data

/**
 * The class represents a collection of [[DataInstance]]] objects that represents its data.
 * @param name the name of the DataSet.
 * @param data the collection of [[DataInstance]] objects that represent its data.
 */
case class ImmutableDataSet(name: String, data: Vector[DataInstance])
