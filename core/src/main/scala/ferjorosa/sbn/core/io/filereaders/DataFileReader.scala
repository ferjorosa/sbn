package ferjorosa.sbn.core.io.filereaders

import ferjorosa.sbn.core.data.{ImmutableDataSet, DataInstance, MutableDataSet}

import scala.util.Try

/**
  * The trait that defines a generic DataFileReader.
  */
trait DataFileReader {

  /**
   *
   * @param path
   * @return
   */
  def loadImmutableDataSet(path: String): Try[ImmutableDataSet]

  /**
   *
   * @param path
   * @return
   */
  def loadMutableDataSet(path: String): MutableDataSet

  /**
   *
   * @param fileName
   * @return
   */
  def doesItReadThisFile(fileName: String): Boolean

}
