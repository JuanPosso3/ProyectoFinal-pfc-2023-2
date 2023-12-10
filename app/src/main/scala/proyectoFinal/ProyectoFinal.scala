/**
  * Taller 3 - Programación Funcional
  * Autores: <Estudiantes>
  * Profesor: Carlos A Delgado
  */
package proyectoFinal

import org.scalameter.measure
import org.scalameter.withWarmer
import org.scalameter.Warmer

object ProyectoFinal{

  def saludo() = "ProyectoFinal"

  def main(args: Array[String]): Unit = {
    println(saludo())
    println(
      withWarmer(new Warmer.Default) measure {
        (1 to 100000000).toArray
      }
    )
  }
 }
