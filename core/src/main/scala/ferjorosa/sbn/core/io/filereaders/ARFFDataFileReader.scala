package ferjorosa.sbn.core.io.filereaders

import java.io.File

import ferjorosa.sbn.core.data._
import ferjorosa.sbn.core.data.attributes._
import ferjorosa.sbn.core.logger.Logging

import scala.io.BufferedSource
import scala.util.{Failure, Success, Try}

/**
  * Created by fer on 27/10/16.
  */
object ARFFDataFileReader extends DataFileReader with Logging{

  override def loadImmutableDataSet(path: String): Try[ImmutableDataSet] = Try{

    val relationName = getRelationName(path) match{
      case Success(name) => name
      case Failure(e) => throw e
    }

    getAttributes(path) match{
      case Success(attributes) => ImmutableDataSet(relationName,getDataInstances(path, attributes))
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

  private def getRelationName(path: String): Try[String] = Try {

    val atRelation: Option[String] = io.Source.fromFile(path).getLines()
      .withFilter(line => !line.isEmpty)
      .withFilter(line => !line.startsWith("%"))
      .find(line => line.startsWith("@relation"))

    if(atRelation.isEmpty)
      throw new IllegalArgumentException("ARFF file does not start with a @relation line.")

    atRelation.get.split(" ")(1)
  }

  private def getAttributes(path: String): Try[Attributes] = Try {
    val attributeLines: List[String] = io.Source.fromFile(path).getLines()
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
        val stateNames = attStates.map(state => state.trim).toList
        Attribute(index, name, FiniteStateSpace(stateNames))

      }else
        throw new IllegalArgumentException("Not able to create an attribute from this line: " + line)
    }

    val optionalAttributeList: List[Try[Attribute]] = attributeLines.zipWithIndex.map{
      case (attLine: String, lineIndex: Int) => createAttributeFromLine(lineIndex, attLine) match {
        case Success(attribute) => Success(attribute)
        case Failure(e) =>
          // Log the error with its associated file line index
          logger.error(path + " (line "+ lineIndex + "): "+ e.getMessage)
          throw e
      }
    }

    if (optionalAttributeList.exists(_.isFailure))
      throw new IllegalArgumentException("ARFF file contains errors in the the @attribute lines.")

    Attributes(optionalAttributeList.map(_.get))
  }

  private def getDataInstances(path: String, attributes: Attributes): Vector[DataInstance] = {

    io.Source.fromFile(path).getLines()
      .withFilter(line => !line.isEmpty)
      .withFilter(line => !line.startsWith("%"))
      .withFilter(line => !line.startsWith("@"))
      .zipWithIndex
      .map{ case (dataLine: String, lineIndex: Int) => DataInstanceFactory.fromARFFDataLine(attributes, dataLine) match {
          case Success(instance) => Success(instance)
          case Failure(e) =>
            // Log the error with its associated file line index
            logger.error(path + " (line "+ lineIndex + ") ignored. An error occurred when trying to create a DataInstance")
            logger.error("Associated error: "+ e.getMessage)
            throw e
        }}
      // Return filtered list of data instances
      .collect{case Success(instance) => instance}
      .toVector
  }
}
