package ferjorosa.sbn.core.io.filereaders

import java.io.File

import ferjorosa.sbn.core.data._
import ferjorosa.sbn.core.data.attributes._

import scala.io.BufferedSource
import scala.util.{Failure, Success, Try}

/**
  * Created by fer on 27/10/16.
  */
object ARFFDataFileReader extends DataFileReader{

  // TODO un dataSet tiene un nombre identificativo, en el caso de provenir de un ARFF, es el @relationName
  override def loadImmutableDataSet(path: String): Try[List[DataInstance]] = Try{
    val bufferedSource = io.Source.fromFile(path)

    val relationName = getRelationName(bufferedSource) match{
      case Success(name) => name
      case Failure(e) => throw e
    }

    getAttributes(bufferedSource) match{
      case Success(attributes) => getDataInstances(bufferedSource, attributes)
      case Failure(e) => throw e
    }
  }

  override def loadMutableDataSet(path: String): MutableDataSet = ???

  override def doesItReadThisFile(fileName: String): Boolean = {
    if (new File(fileName).isDirectory)
      return false

    val parts: Array[String] = fileName.split("\\.")
    parts(parts.length - 1) == "arff"
  }

  // TODO: Si no me equivoco, hace 2 pasadas, una por cada filter, de cualquier forma es mas lento cuanta mas lineas haya en el archivo
  private def getRelationName(bufferedSource: BufferedSource): Try[String] = Try {
    val atRelation: Option[String] = bufferedSource.getLines()
      //.map(line => line.trim)
      .withFilter(line => !line.isEmpty)
      .withFilter(line => !line.startsWith("%"))
      .find(line => line.startsWith("@relation"))

    if(atRelation.isEmpty)
      throw new IllegalArgumentException("ARFF file does not start with a @relation line.")

    atRelation.get.split(" ")(1)
  }

  // TODO: Si no me equivoco, hace 3 pasadas, una por cada filter, de cualquier forma es mas lento cuanta mas lineas haya en el archivo
  private def getAttributes(bufferedSource: BufferedSource): Try[Attributes] = Try {
    val attributeLines: List[String] = bufferedSource.getLines()
      //.map(line => line.trim)
      .withFilter(line => !line.isEmpty)
      .withFilter(line => !line.startsWith("%"))
      .withFilter(line => line.startsWith("@attribute"))
      .toList

    if(attributeLines.isEmpty)
      throw new IllegalArgumentException("ARFF File does not contain @attribute lines.")

    def createAttributeFromLine(index: Int, line: String): Try[Attribute] = Try{
      val parts: Array[String] = line.split("\\s+|\t+")

      var name: String = parts(1).trim
      name = name.replaceAll("^'+", "")
      name = name.replaceAll("'+$", "")

      parts(2) = parts(2).trim

      if (parts(2) == "real" || parts(2) == "numeric") {
        if (parts.length > 3 && parts(3).startsWith("[")) {
          parts(3) = line.substring(line.indexOf("[")).replaceAll("\t", "")
          val min: Double = parts(3).substring(parts(3).indexOf("[") + 1, parts(3).indexOf(",")).toDouble
          val max: Double = parts(3).substring(parts(3).indexOf(",") + 1, parts(3).indexOf("]")).toDouble
          Attribute(index, name, RealStateSpace(min, max))
        }
        else
          Attribute(index, name, new RealStateSpace)

      }else if (parts(2).startsWith("{")) {
        parts(2) = line.substring(line.indexOf("{")).replaceAll("\t", "")
        val attStates = parts(2).substring(1, parts(2).length - 1).split(",")
        val stateNames= attStates.map(state => state.trim).toList
        Attribute(index, name, FiniteStateSpace(stateNames))

      }else if (parts(2) == "SparseMultinomial")
        Attribute(index, name, SparseFiniteStateSpace(parts(3).toInt))

      else
        throw new IllegalArgumentException("Not able to create an attribute from this line: " + line)
    }

    val optionalAttributeList: List[Try[Attribute]] = attributeLines.zipWithIndex.map{
      case (attLine: String, index: Int) => createAttributeFromLine(index, attLine) match {
        case Success(attribute) => Success(attribute)
        case Failure(e) => {
          // log exception with line index
          Failure(e)
        }
      }
    }

    optionalAttributeList.find(_.isFailure) match{
      case Some(failure) => throw new IllegalArgumentException("ARFF file contains errors in the the @attribute lines.")
      case None => Attributes(optionalAttributeList.map(_.get))
    }
  }

  private def getDataInstances(bufferedSource: BufferedSource, attributes: Attributes): List[DataInstance] = {

    bufferedSource.getLines()
      .withFilter(line => !line.isEmpty)
      .withFilter(line => !line.startsWith("%"))
      .withFilter(line => !line.startsWith("@"))
      .map(line =>  DataInstanceFactory.fromARFFDataLine(attributes, line) match {
          case Success(instance) => Success(instance)
          case Failure(e) => {
            // log exception
            Failure(e)
          }
        })
      // Return filtered list of data instances
      .collect{case Success(instance) => instance}
      .toList
  }
}
