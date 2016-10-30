package ferjorosa.sbn.core.data

import java.util.NoSuchElementException

import ferjorosa.sbn.core.data.attributes.{Attributes, FiniteStateSpace, RealStateSpace, SparseFiniteStateSpace}

import scala.collection.mutable.ArrayBuffer
import scala.util.Try


class DataInstance(attributes: Attributes, values: ArrayBuffer[Double])

object DataInstanceFactory {

  @throws[IllegalArgumentException]
  def fromARFFDataLineException(attributes: Attributes, line: String): DataInstance ={
    val values = new ArrayBuffer[Double](attributes.size)
    val parts: Array[String] = line.split(",")

    if (parts.length != attributes.size)
      throw new IllegalArgumentException("The number of columns does not match the number of attributes.")

    for(i <- parts.indices){
      if (parts(i) == "?") values(i) = Double.NaN
      else try{
          attributes.attributeList(i).stateSpaceType match {
            case realStateSpace: RealStateSpace => values(i) = parts(i).toDouble
            case finiteStateSpace: FiniteStateSpace => values(i) = finiteStateSpace.getIndexOfState(parts(i))
            case sparseFiniteStateSpace: SparseFiniteStateSpace => values(i) = parts(i).toInt
          }
        } catch {
            case nfe: NumberFormatException => throw new IllegalArgumentException("Error when reading value: " + parts(i))
            case nse: NoSuchElementException => throw new IllegalArgumentException("Error when reading value: " + parts(i))
        }
    }

    new DataInstance(attributes, values)
  }

  def fromARFFDataLine(attributes: Attributes, line: String): Try[DataInstance] = Try {
    val values = new ArrayBuffer[Double](attributes.size)
    val parts: Array[String] = line.split(",")

    if (parts.length != attributes.size)
      throw new IllegalArgumentException("The number of columns does not match the number of attributes.")

    for(i <- parts.indices) {
      if (parts(i) == "?")
        values(i) = Double.NaN
      else attributes(i).stateSpaceType match {
          case realStateSpace: RealStateSpace => values(i) = parts(i).toDouble
          case finiteStateSpace: FiniteStateSpace => values(i) = finiteStateSpace.getIndexOfState(parts(i))
          case sparseFiniteStateSpace: SparseFiniteStateSpace => values(i) = parts(i).toInt
      }
    }

    new DataInstance(attributes, values)
  }

}