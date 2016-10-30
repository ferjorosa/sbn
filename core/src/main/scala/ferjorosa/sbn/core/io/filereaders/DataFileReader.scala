package ferjorosa.sbn.core.io.filereaders

import ferjorosa.sbn.core.data.{ImmutableDataSet, MutableDataSet}

/**
  * Created by fer on 26/10/16.
  */
trait DataFileReader {

  def loadImmutableDataSet(path: String): ImmutableDataSet

  def loadMutableDataSet(path: String): MutableDataSet

  def doesItReadThisFile(fileName: String): Boolean

}
