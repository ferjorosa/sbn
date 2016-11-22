package sbn.core.io.filereaders

import sbn.core.data.{ImmutableDataSet, MutableDataSet}

import scala.util.Try

/**
  * The trait that defines a generic DataFileReader.
  */
trait DataFileReader {

  /**
    * Tries to load an [[ImmutableDataSet]] from a file path.
    *
    * @param path the path.
    * @return a [[scala.util.Success]]([[ImmutableDataSet]]) or
    *         a [[scala.util.Failure]]([[Exception]]) if an exception occurred during the process.
    */
  def loadImmutableDataSet(path: String): Try[ImmutableDataSet]

  /**
    * Tries to load an [[MutableDataSet]] from a file path.
    *
    * @param path the path.
    * @return a [[scala.util.Success]]([[MutableDataSet]]) or
    *         a [[scala.util.Failure]]([[Exception]]) if an exception occurred during the process.
   */
  def loadMutableDataSet(path: String): MutableDataSet

  /**
    * Tests if this DataFileReader can read the filename.
    *
    * @param fileName the filename.
    * @return true if the filename can be read, false otherwise.
   */
  def doesItReadThisFile(fileName: String): Boolean

}
