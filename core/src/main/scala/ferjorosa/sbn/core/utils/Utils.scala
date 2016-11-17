package ferjorosa.sbn.core.utils

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

  def eqDouble(a: Double, b: Double): Boolean = eqDouble(a, b, 1.11e-15) // IEEE standard fir binary 64

  def eqDouble(a: Double, b: Double, epsilon: Double): Boolean = Math.abs(a/b - 1) < epsilon

}
