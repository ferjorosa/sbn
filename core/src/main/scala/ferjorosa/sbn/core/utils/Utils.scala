package ferjorosa.sbn.core.utils

/**
  * Created by fer on 3/11/16.
  */
object Utils {

  def normalize[T <: Seq[Double]](values: T): T = {
    val sum = values.sum
    values.map(x => x/sum).asInstanceOf[T]
  }
}
