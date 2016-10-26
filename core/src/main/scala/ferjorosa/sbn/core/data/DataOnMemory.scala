package ferjorosa.sbn.core.data

/**
  * Created by fer on 26/10/16.
  */
object DataOnMemory {

  type ImmutableDataOnMemory = scala.collection.immutable.List[DataInstance]

  type MutableDataOnMemory = scala.collection.mutable.ListBuffer[DataInstance]

}
