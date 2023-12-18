/**
  * Proyecto Final - Programación Funcional
  * Autores:  Juan Miguel Posso Alvarado 2259610
  *           Esteban Revelo 2067507
  * Profesor: Carlos A Delgado
  */
package proyectoFinal


import Funciones._
import Funciones_Paralelas._
import Comparacion._

object ProyectoFinal{

  def saludo() = "ProyectoFinal"



  def main(args: Array[String]): Unit = {
      val n = 16
      val cadenaAleatoria = crearAletorias(n)
      println("Tam de la cadena: " + n)
      println("Cadena Aleatoria: " + cadenaAleatoria)
      println(reconstruirCadenaTurboMejorada(n,oraculo(cadenaAleatoria)))
      println(compararAlgoritmos(reconstruirCadenaIngenuo,reconstruirCadenaIngenuoPar(n))(n,oraculo(cadenaAleatoria)))

  }
}