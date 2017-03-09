package sbn.examples.core.test

import sbn.core.data.{DataInstance, ImmutableDataSet}
import sbn.core.io.DataFileLoader

/**
  * Created by equipo on 07/02/2017.
  */
object DiscretizeExample {
  def main(args: Array[String]): Unit = {

    val dataSet = DataFileLoader.loadImmutableDataSet("datasets/discretize/d897_nms30.arff")

    // TODO: Carga mal el dataset, se ve en la primera linea, pero falla en todas, los 6s los carga como 5s

    // Iteramos por cada instancia
    val discretizedValues: Vector[Vector[Double]] = dataSet.get.data.map( instancia => instancia.values.map{ x =>
        if( x== 0) 0
        else if(x == 1 || x == 2) 1.0
        else if(x == 3 || x == 4) 2.0
        else if(x == 5 || x == 6) 3.0
        else if(x == 7 || x == 8) 4.0
        else if(x == 9 || x == 10) 5.0
        else 6.0
    })

    val discretizedInstances = discretizedValues.map( x => DataInstance(dataSet.get.attributes, x))

    val newDataSet = ImmutableDataSet("new_d897_nms30", dataSet.get.attributes, discretizedInstances)

    println("START")
    newDataSet.data.foreach(inst => {
      val s = inst.values.map(v => v.toInt )
      println(s)
    })
  }
}
