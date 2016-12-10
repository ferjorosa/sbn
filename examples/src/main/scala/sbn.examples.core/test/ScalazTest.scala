package sbn.examples.core.test

import breeze.linalg.DenseVector
import sbn.core.variables.{Assignments, Assignment}

import scalaz._
import scalaz.syntax.semigroup._

/**
 * Created by Fernando on 09/12/2016.
 */
object ScalazTest {

  implicit val SemigroupDenseVector: Semigroup[DenseVector[Double]] = Semigroup.instance((a, b) => a + b)

  def main (args: Array[String]){
    /*val a = Map(Assignments(Set.empty[Assignment]) -> DenseVector(0.0, 0.0, 1.0))
    val b = Map(Assignments(Set.empty[Assignment]) -> DenseVector(1.0, 0.0, 0.0))
    val c = Map(Assignments(Set.empty[Assignment]) -> 0.0)
    val d = Map(Assignments(Set.empty[Assignment]) -> 1.0)

    val vect = Vector(a, b)
    val vect2 = Vector(c,d)
*/
   // vect2.reduceLeft(_ |+| _)

    val a = (Assignments(Set.empty[Assignment]), 2.0)
    val b = (Assignments(Set.empty[Assignment]), 3.0)
    val vect = Vector(a, b)
    val sum = vect.reduceLeft{(a , b) => (a._1, a._2 + b._2)}

    val c = (2, DenseVector(1.0, 0.0, 0.0))
    val d = (1, DenseVector(1.0, 0.0, 1.0))
    val e = (1, DenseVector(1.0, 0.0, 2.0))
    val vect2 = Vector(c, d, e)
    val sum2 = vect2.reduceLeft{(a , b) => (a._1, a._2 + b._2)}

    val sum3= vect2.groupBy(_._1).map {case (k, v) => k -> v}//.reduceLeft{(a , b) => (a._1, a._2 + b._2)}}.values
   // val sum3:Int = vect2.groupBy(_._1).map {x => x}

    val x = 0
  }

 /* private def combineMap(a: Map[Assignments, DenseVector[Double]], b: Map[Assignments, DenseVector[Double]]): Map[Assignments, DenseVector[Double]] = {
    b.keys.map{k=>
      if(a.contains(k))

    }
  }*/
}
