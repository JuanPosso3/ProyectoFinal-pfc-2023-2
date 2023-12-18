package proyectoFinal
import org.scalameter.measure
import org.scalameter.Quantity
import org.scalameter.withWarmer
import org.scalameter.Warmer
import Funciones._
import Funciones_Paralelas._
object Comparacion {
  def compararAlgoritmos(Funcion1:(Int,Oraculo) => Seq[Char], Funcion2:(Int,Oraculo) => Seq[Char])(n: Int,oraculo: Oraculo): (Double, Double, Double) = {
    val timeF1 = withWarmer(new Warmer.Default) measure {
      Funcion1(n, oraculo)
    }
    val timeF2 = withWarmer(new Warmer.Default) measure {
      Funcion2(n,oraculo)
    }

    val promedio = timeF1.value / timeF2.value
    (timeF1.value, timeF2.value, promedio)

    
  }

  def desempenoDeFunciones(tamanoCadena: Int): Vector[Double] = {
    println("Tamanio: " + tamanoCadena)
    val cadenaAleatoria = crearAletorias(tamanoCadena)
    val oraculo = Funciones.oraculo(cadenaAleatoria)

    val tiemposIngenua = (1 to 100).map(_ => 0.0).toArray
    for (i <- 0 until 100) {
      val time = withWarmer(new Warmer.Default) measure {
        reconstruirCadenaIngenuo(cadenaAleatoria.length, oraculo)
      }
      tiemposIngenua(i) = time.value
    }

    val tiemposIngenuaPar = (1 to 100).map(_ => 0.0).toArray
    for (i <- 0 until 100) {
      val time = withWarmer(new Warmer.Default) measure {
        reconstruirCadenaIngenuoPar(5)(cadenaAleatoria.length, oraculo)
      }
      tiemposIngenuaPar(i) = time.value
    }

    val tiempoMejorado = (1 to 100).map(_ => 0.0).toArray
    for (i <- 0 until 100) {
      val time = withWarmer(new Warmer.Default) measure {
        reconstruirCadenaMejorado(cadenaAleatoria.length, oraculo)
      }
      tiempoMejorado(i) = time.value
    }

    val tiempoMejoradoPar = (1 to 100).map(_ => 0.0).toArray
    for (i <- 0 until 100) {
      val time = withWarmer(new Warmer.Default) measure {
        reconstruirCadenaMejoradoPar(5)(cadenaAleatoria.length, oraculo)
      }
      tiempoMejoradoPar(i) = time.value
    }

    val tiempoTurbo = (1 to 100).map(_ => 0.0).toArray
    for (i <- 0 until 100) {
      val time = withWarmer(new Warmer.Default) measure {
        reconstruirCadenaTurbo(cadenaAleatoria.length, oraculo)
      }
      tiempoTurbo(i) = time.value
    }

    val tiempoTurboPar = (1 to 100).map(_ => 0.0).toArray
    for (i <- 0 until 100) {
      val time = withWarmer(new Warmer.Default) measure {
        reconstruirCadenaTurboPar(5)(cadenaAleatoria.length, oraculo)
      }
      tiempoTurboPar(i) = time.value
    }

    val tiempoTurboMejorada = (1 to 100).map(_ => 0.0).toArray
    for (i <- 0 until 100) {
      val time = withWarmer(new Warmer.Default) measure {
        reconstruirCadenaTurboMejorada(cadenaAleatoria.length, oraculo)
      }
      tiempoTurboMejorada(i) = time.value
    }

    val tiempoTurboMejoradaPar = (1 to 100).map(_ => 0.0).toArray
    for (i <- 0 until 100) {
      val time = withWarmer(new Warmer.Default) measure {
        reconstruirCadenaTurboMejoradaPar(5)(cadenaAleatoria.length, oraculo)
      }
      tiempoTurboMejoradaPar(i) = time.value
    }

    val tiempoTurboAcelerada = (1 to 100).map(_ => 0.0).toArray
    for (i <- 0 until 100) {
      val time = withWarmer(new Warmer.Default) measure {
        reconstruirCadenaTurboAcelerada(cadenaAleatoria.length, oraculo)
      }
      tiempoTurboAcelerada(i) = time.value
    }
    val tiempoTurboAceleradaPar = (1 to 100).map(_ => 0.0).toArray
    for (i <- 0 until 100) {
      val time = withWarmer(new Warmer.Default) measure {
        reconstruirCadenaTurboAceleradaPar(5)(cadenaAleatoria.length, oraculo)
      }
      tiempoTurboAceleradaPar(i) = time.value
    }
    Vector(tiemposIngenua.sum / 100, tiemposIngenuaPar.sum / 100, tiempoMejorado.sum / 100,
      tiempoMejoradoPar.sum / 100, tiempoTurbo.sum / 100, tiempoTurboPar.sum / 100,
      tiempoTurboMejorada.sum / 100, tiempoTurboMejoradaPar.sum / 100,
      tiempoTurboAcelerada.sum / 100, tiempoTurboAceleradaPar.sum / 100)
  }

  def desempenoDeFuncionesSec(tamCadena: Int): Vector[Double] = {
    val cadenaAleatoria = crearAletorias(tamCadena)
    val oraculo = Funciones.oraculo(cadenaAleatoria)

    val tiempoIngenua = (1 to 100).map(_ => 0.0).toArray
    for (i <- 0 until 100) {
      val time = withWarmer(new Warmer.Default) measure {
        reconstruirCadenaIngenuo(cadenaAleatoria.length, oraculo)
      }
      tiempoIngenua(i) = time.value
    }

    val tiempoMejorado = (1 to 100).map(_ => 0.0).toArray
    for (i <- 0 until 100) {
      val time = withWarmer(new Warmer.Default) measure {
        reconstruirCadenaMejorado(cadenaAleatoria.length, oraculo)
      }
      tiempoMejorado(i) = time.value
    }

    val tiempoTurbo = (1 to 100).map(_ => 0.0).toArray
    for (i <- 0 until 100) {
      val time = withWarmer(new Warmer.Default) measure {
        reconstruirCadenaTurbo(cadenaAleatoria.length, oraculo)
      }
      tiempoTurbo(i) = time.value
    }
    val tiempoTurboMejorada = (1 to 100).map(_ => 0.0).toArray
    for (i <- 0 until 100) {
      val time = withWarmer(new Warmer.Default) measure {
        reconstruirCadenaTurboMejorada(cadenaAleatoria.length, oraculo)
      }
      tiempoTurboMejorada(i) = time.value
    }
    val tiempoTurboAcelerada = (1 to 100).map(_ => 0.0).toArray
    for (i <- 0 until 100) {
      val time = withWarmer(new Warmer.Default) measure {
        reconstruirCadenaTurboAcelerada(cadenaAleatoria.length, oraculo)
      }
      tiempoTurboAcelerada(i) = time.value
    }

    Vector(tiempoIngenua.sum / 100,  tiempoMejorado.sum / 100, tiempoTurbo.sum / 100,
      tiempoTurboMejorada.sum / 100, tiempoTurboAcelerada.sum / 100)
  }

  def desempenoDeFuncionesParalelas(tamCadena: Int): Vector[Double] = {
    val cadenaAleatoria = crearAletorias(tamCadena)
    val oraculo = Funciones.oraculo(cadenaAleatoria)

    val tiempoIngenuaPar = (1 to 100).map(_ => 0.0).toArray
    for (i <- 0 until 100) {
      val time = withWarmer(new Warmer.Default) measure {
        reconstruirCadenaIngenuoPar(5)(cadenaAleatoria.length, oraculo)
      }
      tiempoIngenuaPar(i) = time.value
    }

    val tiempoMejoradoPar = (1 to 100).map(_ => 0.0).toArray
    for (i <- 0 until 100) {
      val time = withWarmer(new Warmer.Default) measure {
        reconstruirCadenaMejoradoPar(5)(cadenaAleatoria.length, oraculo)
      }
      tiempoMejoradoPar(i) = time.value
    }

    val tiempoTurboPar = (1 to 100).map(_ => 0.0).toArray
    for (i <- 0 until 100) {
      val time = withWarmer(new Warmer.Default) measure {
        reconstruirCadenaTurboPar(5)(cadenaAleatoria.length, oraculo)
      }
      tiempoTurboPar(i) = time.value
    }
    val tiempoTurboMejoradaPar = (1 to 100).map(_ => 0.0).toArray
    for (i <- 0 until 100) {
      val time = withWarmer(new Warmer.Default) measure {
        reconstruirCadenaTurboMejoradaPar(5)(cadenaAleatoria.length, oraculo)
      }
      tiempoTurboMejoradaPar(i) = time.value
    }
    val tiempoTurboAceleradaPar = (1 to 100).map(_ => 0.0).toArray
    for (i <- 0 until 100) {
      val time = withWarmer(new Warmer.Default) measure {
        reconstruirCadenaTurboAceleradaPar(5)(cadenaAleatoria.length, oraculo)
      }
      tiempoTurboAceleradaPar(i) = time.value
    }

    Vector(tiempoIngenuaPar.sum / 100,  tiempoMejoradoPar.sum / 100, tiempoTurboPar.sum / 100,
      tiempoTurboMejoradaPar.sum / 100, tiempoTurboAceleradaPar.sum / 100)
  }

}