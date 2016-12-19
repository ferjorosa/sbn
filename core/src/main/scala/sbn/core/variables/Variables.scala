package sbn.core.variables

/**
  * Created by fer on 19/12/16.
  */
case class Variables[T <: Variable](variables) extends Iterable[T]{


  /**
    * Returns a custom iterator for the collection.
    *
    * @return a custom iterator for the collection.
    */
  override def iterator: Iterator[T] = orderedAttributeList.iterator
}
