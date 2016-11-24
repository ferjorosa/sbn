package ferjorosa.sbn.examples.core.io

import sbn.core.data.ImmutableDataSet
import sbn.core.io.DataFileLoader

import scala.util.{Failure, Success, Try}

/**
 * Created by Fernando on 30/10/2016.
 */
object DataFileLoaderExample {

  def main(args: Array[String]) {

    val init = System.currentTimeMillis()
    for(i <- 0 to 10000) {
      val daterino = loadAlarmFile match {
        case Success(data) => data
        case Failure(e) => throw e
      }
    }
    val fin = System.currentTimeMillis()
    println((fin-init) + "ms")
  }

  private def loadAlarmFile: Try[ImmutableDataSet] = {
    DataFileLoader.loadImmutableDataSet("datasets/ferjorosaData/Alarm_train.arff")
  }

  private def loadAsiaFile: Try[ImmutableDataSet] = {
    DataFileLoader.loadImmutableDataSet("datasets/ferjorosaData/Asia_train.arff")
  }

  private def loadCsvFile: Try[ImmutableDataSet] = {
    DataFileLoader.loadImmutableDataSet("datasets/ferjorosaData/sprinklerData300.csv")
  }

}
