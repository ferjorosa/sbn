package ferjorosa.sbn.core.io

import java.io.File

import ferjorosa.sbn.core.data.{DataInstance, ImmutableDataSet, MutableDataSet}
import ferjorosa.sbn.core.io.filereaders.{ARFFDataFileReader, DataFileReader}

import scala.util.{Failure, Success, Try}

/**
  * This singleton class should be used to load data files as dataSets. It will choose the adequate reader for the file
  * specified by the user. If that file is not supported a Failure will be returned instead.
  */
object DataFileLoader {

  /**
    * Tries to load an [[ImmutableDataSet]] with the right reader from a specific path.
    * @param path the path specified by the user.
    * @return a [[Success]]([[ImmutableDataSet]]) or
    *         a [[Failure]]([[Exception]]) if an exception occurred during the process.
    */
  def loadImmutableDataSet(path: String): Try[ImmutableDataSet] =  selectDataFileReader(path) match{
      case Success(fileReader) => fileReader.loadImmutableDataSet(path)
      case Failure(e) => Failure(e)
  }

  def loadMutableDataSet(path: String): MutableDataSet = ???

  /**
    * Analyzes the extension of the provided file and chooses the right reader for the job if available.
    * @param fileName the name of the chosen file.
    * @return a [[Success]]([[DataFileReader]]) or
    *         a [[Failure]]([[Exception]]) if the file extension not supported.
    */
  private def selectDataFileReader(fileName: String): Try[DataFileReader] = Try{
    if (new File(fileName).isDirectory)
      throw new IllegalArgumentException("The path refers to a directory, which is not supported yet")

    val parts: Array[String] = fileName.split("\\.")
    parts(parts.length - 1) match{
      case "arff" => ARFFDataFileReader
      case _ => throw new IllegalArgumentException("File extension not supported")
    }
  }

}
