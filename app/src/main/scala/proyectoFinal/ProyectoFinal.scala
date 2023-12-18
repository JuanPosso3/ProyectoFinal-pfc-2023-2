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


  def main(args: Array[String]): Unit = {
      val n = 10
      val cadenaAleatoria = crearAletorias(n)
      println("Tam de la cadena: " + n)
      println("Cadena Aleatoria: " + cadenaAleatoria)
      println(reconstruirCadenaTurbo(n,oraculo(cadenaAleatoria)))
      println(compararAlgoritmos(reconstruirCadenaTurbo,reconstruirCadenaTurboPar(n))(n,oraculo(cadenaAleatoria)))

  }
}