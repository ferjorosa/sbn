package sbn.core.io.filereaders

import java.io.File

import sbn.core.data._
import sbn.core.data.attributes._
import sbn.core.logger.Logging

import scala.util.{Failure, Success, Try}

/**
  * The DataFileReader for ARFF files.
  */
object ARFFDataFileReader extends DataFileReader with Logging{

  /**
    * Tries to load an [[ImmutableDataSet]] from an ARFF file.
    *
    * @param path the path to the ARFF file.
    * @return a [[Success]]([[ImmutableDataSet]]) or
    *         a [[Failure]]([[Exception]]) if an exception occurred during the process.
    */
  override def loadImmutableDataSet(path: String): Try[ImmutableDataSet] = Try{

    val relationName = getRelationName(path) match{
      case Success(name) => name
      case Failure(e) => throw e
    }

    getAttributes(path) match{
      case Success(attributes) => ImmutableDataSet(relationName, attributes, getDataInstances(path, attributes))
      case Failure(e) => throw e
    }

  }

  // TODO
  override def loadMutableDataSet(path: String): MutableDataSet = ???

  /**
   * Tests if this DataFileReader can read the filename.
    *
   * @param fileName the filename.
   * @return true if the filename can be read, false otherwise.
   */
  override def doesItReadThisFile(fileName: String): Boolean = {
    if (new File(fileName).isDirectory)
      return false

    val parts: Array[String] = fileName.split("\\.")
    parts(parts.length - 1) == "arff"
  }

  /**
   * This method reads the file and tries to return the @relation string.
    *
   * @param path the path to the file, needed to create a reading source.
   * @return a [[Success]]([[String]]) or a [[Failure]]([[Exception]]) if an exception occurred during the process.
   */
  private def getRelationName(path: String): Try[String] = Try {

    val atRelation: Option[String] = io.Source.fromFile(path).getLines()
      .withFilter(line => !line.isEmpty)
      .withFilter(line => !line.startsWith("%"))
      .find(line => line.startsWith("@relation"))

    if(atRelation.isEmpty)
      throw new IllegalArgumentException("ARFF file does not start with a @relation line")

    atRelation.get.split(" ")(1)
  }

  /**
   * This method reads the file and tries to generate a [[Attributes]] object containing all the [[Attribute]] objects
   * that refer to the @attribute lines.
    *
   * @param path the path to the file, needed to create a reading source.
   * @return a [[Success]]([[Attributes]]) or a [[Failure]]([[Exception]]) if an exception occurred during the process.
   */
  private def getAttributes(path: String): Try[Attributes] = Try {
    val attributeLines: List[String] = io.Source.fromFile(path).getLines()
      .withFilter(line => !line.isEmpty)
      .withFilter(line => !line.startsWith("%"))
      .withFilter(line => line.startsWith("@attribute"))
      .toList

    if(attributeLines.isEmpty)
      throw new IllegalArgumentException("ARFF File does not contain @attribute lines")

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
          Attribute(name, RealStateSpace(min, max))
        }
        else
          Attribute(name, RealStateSpace())

      }else if (parts(2).startsWith("{")) {
        parts(2) = line.substring(line.indexOf("{")).replaceAll("\t", "")
        val attStates = parts(2).substring(1, parts(2).length - 1).split(",")
        val stateNames = attStates.map(state => state.trim).toVector
        Attribute(name, FiniteStateSpace(stateNames))

      }else
        throw new IllegalArgumentException("Not able to create an attribute from this line: " + line)
    }

    val optionalAttributeList: List[Try[Attribute]] = attributeLines.zipWithIndex.map{
      case (attLine: String, lineIndex: Int) => createAttributeFromLine(lineIndex, attLine) match {
        case Success(attribute) => Success(attribute)
        case Failure(e) =>
          // Log the error with its associated file line index
          logger.error(path + " (attribute line "+ lineIndex + "): "+ e.getMessage)
          throw e
      }
    }

    if (optionalAttributeList.exists(_.isFailure))
      throw new IllegalArgumentException("ARFF file contains errors in the the @attribute lines")

    Attributes(optionalAttributeList.map(_.get))
  }

  /**
   * This method reads the file and returns a [[Vector]] of [[DataInstance]] objects containing all the ones that
   * have been created without errors.
    *
   * @param path the path to the file, needed to create a reading source.
   * @param attributes the [[Attributes]] object needed to create a [[DataInstance]]
   * @return the [[Vector]] containing the [[DataInstance]] objects that have been created without errors.
   */
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
            logger.error(path + " (data line "+ lineIndex + ") ignored. An error occurred when trying to create a DataInstance")
            logger.error("Associated error: "+ e.getMessage)
            throw e
        }}
      // Return filtered list of data instances
      .collect{case Success(instance) => instance}
      .toVector
  }
}
