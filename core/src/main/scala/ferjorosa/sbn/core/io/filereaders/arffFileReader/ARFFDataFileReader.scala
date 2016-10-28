package ferjorosa.sbn.core.io.filereaders.arffFileReader

import java.io.File
import java.nio.file.Paths

import ferjorosa.sbn.core.data._
import ferjorosa.sbn.core.io.filereaders.DataFileReader
import ferjorosa.sbn.core.variables.{FiniteStateSpace, RealStateSpace, SparseFiniteStateSpace}

import scala.io.BufferedSource

/**
  * Created by fer on 27/10/16.
  */
object ARFFDataFileReader extends DataFileReader{

  @throws[IllegalArgumentException]
  override def loadImmutableDataSet(path: String): ImmutableDataSet = {
    val bufferedSource = io.Source.fromFile(path)

    val relationName = getRelationName(bufferedSource)
    val attributes = getAttributes(bufferedSource)


  }
  // TODO: Si no me equivoco, hace 2 pasadas, una por cada filter, de cualquier forma es mas lento cuanta mas lineas haya en el archivo
  @throws[IllegalArgumentException]
  private def getRelationName(bufferedSource: BufferedSource): String = {
    val atRelation: Option[String] = bufferedSource.getLines()
      //.map(line => line.trim)
      .filter(line => !line.isEmpty)
      .filter(line => !line.startsWith("%"))
      .find(line => line.startsWith("@relation"))

    if(atRelation.isEmpty)
      throw new IllegalArgumentException("ARFF file does not start with a @relation line.")

    atRelation.get.split(" ")(1)
  }

  // TODO: Si no me equivoco, hace 3 pasadas, una por cada filter, de cualquier forma es mas lento cuanta mas lineas haya en el archivo
  @throws[IllegalArgumentException]
  private def getAttributes(bufferedSource: BufferedSource): Attributes = {
    val attributeLines: List[String] = bufferedSource.getLines()
      //.map(line => line.trim)
      .filter(line => !line.isEmpty)
      .filter(line => !line.startsWith("%"))
      .filter(line => line.startsWith("@attribute"))
      .toList

    val attributeList = attributeLines
      .zipWithIndex
      .map{case (attLine: String, index: Int) => createAttributeFromLine(index, attLine)}

    def createAttributeFromLine(index: Int, line: String): Attribute = {
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
          return Attribute(index, name, RealStateSpace(min, max))
        }
        else
          return Attribute(index, name, new RealStateSpace)

      }else if (parts(2).startsWith("{")) {
        parts(2) = line.substring(line.indexOf("{")).replaceAll("\t", "")
        val attStates = parts(2).substring(1, parts(2).length - 1).split(",")
        val stateNames= attStates.map(state => state.trim).toList
        return Attribute(index, name, FiniteStateSpace(stateNames))

      }else if (parts(2) == "SparseMultinomial")
        return Attribute(index, name, SparseFiniteStateSpace(parts(3).toInt))

      else
        throw new IllegalArgumentException("Not able to create an attribute from this line: " + line)
    }

    Attributes(attributeList)
  }

  @throws[IllegalArgumentException]
  private def getDataInstances(bufferedSource: BufferedSource, attributes: Attributes): List[DataInstance] = {
    val dataInstanceList = List.empty[DataInstance]

    val dataRows: List[DataRowWeka] = bufferedSource.getLines()
      .filter(line => !line.isEmpty)
      .filter(line => !line.startsWith("%"))
      .filter(line => !line.startsWith("@"))
      .map(line => new DataRowWeka(attributes, line))
      .toList
  }

  override def loadMutableDataSet(path: String): MutableDataSet = ???

  override def close(): Unit = ???

  override def restart(): Unit = ???

  override def doesItReadThisFile(fileName: String): Boolean = {
    if (new File(fileName).isDirectory)
      return false

    val parts: Array[String] = fileName.split("\\.")
    return parts(parts.length - 1) == "arff"
  }
}
