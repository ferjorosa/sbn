package ferjorosa.sbn.core.io

import java.io.File

import ferjorosa.sbn.core.data.{DataInstance, ImmutableDataSet, MutableDataSet}
import ferjorosa.sbn.core.io.filereaders.{ARFFDataFileReader, DataFileReader}

import scala.util.{Failure, Success, Try}

/**
  * Created by fer on 27/10/16.
  */
object DataFileLoader {

  /**
   *
   * @param path
   * @return
   */
  def loadImmutableDataSet(path: String): Try[ImmutableDataSet] =  selectDataFileReader(path) match{
      case Success(fileReader) => fileReader.loadImmutableDataSet(path)
      case Failure(e) => throw e
  }

  def loadMutableDataSet(path: String): MutableDataSet = ???

  private def selectDataFileReader(fileName: String): Try[DataFileReader] = Try{
    if (new File(fileName).isDirectory)
      throw new IllegalArgumentException("the path refers to a directory, which is not supported yet.")

    val parts: Array[String] = fileName.split("\\.")
    parts(parts.length - 1) match{
      case "arff" => ARFFDataFileReader
      case _ => throw new IllegalArgumentException("File extension not supported.")
    }

  }

}
