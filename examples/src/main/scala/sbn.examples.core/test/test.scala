package sbn.examples.core.test

import sbn.core.data.attributes.FiniteStateSpace
import sbn.core.variables.{Assignment, Assignments, Variable, ModelVariablesFactory}

/**
  * Created by fer on 3/11/16.
  */
object test {

  def main(args: Array[String]): Unit = {

    val multinomial_1 = ModelVariablesFactory.newMultinomialVariable("multinomial_1", 2)
    val multinomial_2 = ModelVariablesFactory.newMultinomialVariable("multinomial_2", 2)
    val multinomial_3 = ModelVariablesFactory.newMultinomialVariable("multinomial_3", 3)
    val variable = ModelVariablesFactory.newMultinomialVariable("variable", 3)
    val parents: Set[Variable] = Set(multinomial_1, multinomial_2, multinomial_3)

    val s  = Seq("0","1")
    val t  = Seq("0","1")
    val u  = Seq("0","1","2")
    val sq = Seq(s,t,u)

    val s1  = Seq(0, 1)
    val t1  = Seq(0, 1)
    val u1 = Seq(0, 1, 2)
    val sq1 = Seq(s1,t1,u1)

    val newSequence = cartesianProduct(sq)
    //newSequence.map(_.toVector).foreach(println)

    val newIntSequence = cartesianProduct(sq1)
    //newSequence.map(_.toVector).foreach(println)

    val statesSequence = generatePossibleIndexes(parents.toList)
    //statesSequence.map(_.toVector).foreach(println)

    val assignmentSequence = generatePossibleAssignments(parents.toList)
    //assignmentSequence.map(_.toVector).foreach(println)
    val assignments = assignmentSequence.map(x => Assignments(x.toSet))
    val assignmentsList = assignments.toList
    assignmentsList.foreach(println)
  }

  private def mostrarCombinacion(array: Array[Int]) {
    var combinacion: String = ""

    for(i<-array.indices){
      combinacion += array(i) + " "
    }
    println(combinacion)
  }

  def cartesianProduct[A](xs: Traversable[Traversable[A]]): Seq[Seq[A]] = {
    xs.foldLeft(Seq(Seq.empty[A])){
      (x, y) => for (a <- x.view; b <- y) yield a :+ b }
  }

  def generatePossibleIndexes(variables: Seq[Variable]): Seq[Seq[Int]] = {
    val stateSequences: Seq[Vector[Int]] = variables.map(v => v.attribute.stateSpaceType match {
      case finite: FiniteStateSpace => finite.stateIndexes
    })
    cartesianProduct(stateSequences)
  }

  def generatePossibleAssignments(variables: Seq[Variable]): Seq[Seq[Assignment]] = {
    val stateSequences: Seq[Vector[Int]] = variables.map(v => v.attribute.stateSpaceType match {
      case finite: FiniteStateSpace => finite.stateIndexes
    })

    val zippedCombinations = cartesianProduct(stateSequences).map(stateCombination => variables.zip(stateCombination))

    zippedCombinations.map{
      combination => combination. map(variableAndValue => Assignment(variableAndValue._1, variableAndValue._2))
    }
  }

}
