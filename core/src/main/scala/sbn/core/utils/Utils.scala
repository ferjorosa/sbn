package sbn.core.utils

import org.apache.commons.math3.util.FastMath

/**
  * Created by fer on 3/11/16.
  */
object Utils {

  def normalize[T <: Seq[Double]](values: T): T = {
    val sum = values.sum
    values.map(x => x/sum).asInstanceOf[T]
  }

  def cartesianProduct[A](xs: Traversable[Traversable[A]]): Seq[Seq[A]] = {
    xs.foldLeft(Seq(Seq.empty[A])){
      (x, y) => for (a <- x.view; b <- y) yield a :+ b }
  }

  def eqDouble(a: Double, b: Double): Boolean = eqDouble(a, b, 1.11e-14) // IEEE standard for binary 64 (technically is 1.11e-16, but i found some problems with e-16 and with e-15)

  def eqDouble(a: Double, b: Double, epsilon: Double): Boolean = FastMath.abs(a-b) < epsilon


}
