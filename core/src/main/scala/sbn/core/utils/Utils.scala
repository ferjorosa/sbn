package sbn.core.utils

import org.apache.commons.math3.util.FastMath

/**
  * utility class with some useful method used all across the library.
  */
object Utils {

  /**
    *
    * @param values
    * @tparam T
    * @return
    */
  def normalize[T <: Seq[Double]](values: T): T = {
    val sum = values.sum
    values.map(x => x/sum).asInstanceOf[T]
  }

  /**
    * Returns the cartesian product
    *
    * @param xs
    * @tparam A
    * @return
    */
  def cartesianProduct[A](xs: Traversable[Traversable[A]]): Seq[Seq[A]] = {
    xs.foldLeft(Seq(Seq.empty[A])){
      (x, y) => for (a <- x.view; b <- y) yield a :+ b }
  }

  /**
    *
    * @param a
    * @param b
    * @return
    */
  // IEEE standard for binary 64 (technically is 1.11e-16, but i found some problems with e-16 and with e-15)
  def eqDouble(a: Double, b: Double): Boolean = eqDouble(a, b, 1.11e-14)

  /**
    *
    * @param a
    * @param b
    * @param epsilon
    * @return
    */
  def eqDouble(a: Double, b: Double, epsilon: Double): Boolean = FastMath.abs(a-b) < epsilon


}
