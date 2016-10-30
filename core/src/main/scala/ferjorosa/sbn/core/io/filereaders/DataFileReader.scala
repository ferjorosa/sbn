package ferjorosa.sbn.core.io.filereaders

import ferjorosa.sbn.core.data.{DataInstance, MutableDataSet}

import scala.util.Try

/**
  * Created by fer on 26/10/16.
  */
trait DataFileReader {

  def loadImmutableDataSet(path: String): Try[List[DataInstance]]

  def loadMutableDataSet(path: String): MutableDataSet

  def doesItReadThisFile(fileName: String): Boolean

}
