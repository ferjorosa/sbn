package ferjorosa.sbn.core.io

import ferjorosa.sbn.core.data.{ImmutableDataSet, MutableDataSet}
import ferjorosa.sbn.core.data.filereaders.DataFileReader
import ferjorosa.sbn.core.data.filereaders.arffFileReader.ARFFDataFileReader

/**
  * Created by fer on 27/10/16.
  */
object DataFileLoader {

  def loadDataSet(path: String): ImmutableDataSet = {
    val dataFileReader = selectDataFileReader(path)
    dataFileReader.loadImmutableDataSet(path)

  }

  def loadMutableDataSet(path: String): MutableDataSet = {
    null
  }

  private def selectDataFileReader(fileName: String): DataFileReader = {

  }

}
