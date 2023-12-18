/**
  * Proyecto Final - Programación Funcional
  * Autores:  Juan Miguel Posso Alvarado 2259610
  *           Esteban Revelo 2067507
  *           Nicolas Rojas 2259
  * Profesor: Carlos A Delgado
  */
package proyectoFinal

import org.scalameter.measure
import org.scalameter.withWarmer
import org.scalameter.Warmer
import Funciones._
import Funciones_Paralelas._
import Comparacion._

object ProyectoFinal{

  def saludo() = "ProyectoFinal"



  def main(args: Array[String]): Unit = {
      // Pruebas de la función oraculoFunc
      val n = 4
      val cadenaAleatoria = crearAletorias(n)
      println("Tamaño de la cadena: " + n)
      println("Cadena Aleatoria: " + cadenaAleatoria)
      println(compararAlgoritmos(reconstruirCadenaIngenuo, reconstruirCadenaIngenuoPar(5))(n, oraculo(cadenaAleatoria)))

  }
}

