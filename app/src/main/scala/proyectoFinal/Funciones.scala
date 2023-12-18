package proyectoFinal
import scala.util.Random

object Funciones {

  val alfabeto= Seq('a','c','g','t')
  type Oraculo = Seq [ Char ] => Boolean
  def oraculo(cadena: Seq[Char]): Oraculo = {
    (subCadena: Seq[Char]) => cadena.mkString.contains(subCadena.mkString)
  }
  def crearAletorias(tam: Int): String = {
    val cadenaA: String = (for {
      i <- 1 to tam
      } yield alfabeto(Random.nextInt(alfabeto.length))).mkString
      cadenaA
  }
  def generarCadenas(tamano: Int): Seq[String] = {
    if (tamano == 0) {
      Seq("")
    } else {
      alfabeto.flatMap { caracter =>
        generarCadenas(tamano - 1).map(combinacion => caracter + combinacion)
      }
    }
  }

  def reconstruirCadenaIngenuo(n: Int, o: Oraculo): Seq[Char] = {
    val combinaciones = generarCadenas(n)
    combinaciones.find(cadena => o(cadena)).get.toSeq
  }


  def reconstruirCadenaMejorado (n : Int , oraculo : Oraculo ): Seq[Char]= {
    def reconstruirCadenaMejoradoAux( acu: Seq[String], n:Int): Seq[String] = {
      if (n == 0) {
        acu
      } else {
        val combinaciones  = generarCadenas(n)

        val cadenaEncontrada = for {
          cadena <- combinaciones
          if oraculo(cadena)
        } yield cadena

        val acumulado = acu ++ cadenaEncontrada
        reconstruirCadenaMejoradoAux( acumulado,n-1)
      }
    }

    if(n%2 == 0){
      val subCadenasCorrectas = reconstruirCadenaMejoradoAux(Seq(), n/2)

      val posiblesCadenas =  for {
        subCadena1 <- subCadenasCorrectas
        subCadena2 <- subCadenasCorrectas
        if oraculo(subCadena1 + subCadena2) && ((subCadena1 + subCadena2).length == n)
      } yield subCadena1 + subCadena2

      posiblesCadenas.mkString
    }else{
      val subCadenasCorrectas1 = reconstruirCadenaMejoradoAux(Seq(), n/2)
      val subCadenasCorrectas2 = reconstruirCadenaMejoradoAux(Seq(), n-(n/2))
      val posiblesCadenas = for {
        subCadena1 <- subCadenasCorrectas1
        subCadena2 <- subCadenasCorrectas2
        if oraculo(subCadena1 + subCadena2) && ((subCadena1 + subCadena2).length == n)
      } yield subCadena1 + subCadena2

      posiblesCadenas.mkString

    }
  }


  def reconstruirCadenaTurbo (n : Int , o : Oraculo ) : Seq [Char]={
      def reconstruirCadenaTurboAux(acu: Seq[String], n:Int): Seq[String] = {
        if (((n == 0 || n == 1) && acu.nonEmpty) || n < 0 ) {
          acu
        } else {
          val combinaciones  = generarCadenas(n)

          val cadenaSelecta = for {
            cadena <- combinaciones
            if o(cadena)
          } yield cadena

          val acumulado = acu ++ cadenaSelecta
          reconstruirCadenaTurboAux(acumulado,n-2)
        }
      }
      if(n%2 == 0){
        val subCadenasBuenas= reconstruirCadenaTurboAux(Seq(), n/2)

        val posiblesCadenas =  for {
          subCadena1 <- subCadenasBuenas
          subCadena2 <- subCadenasBuenas
          if o(subCadena1 + subCadena2) && ((subCadena1 + subCadena2).length == n)
        } yield subCadena1 + subCadena2

        posiblesCadenas.mkString
      }else{
        val subCadenasBuenas1 = reconstruirCadenaTurboAux(Seq(), n/2)
        val subCadenasBuenas2 = reconstruirCadenaTurboAux(Seq(), n-(n/2))
        val posiblesCadenas = for {
          subCadena1 <- subCadenasBuenas1
          subCadena2 <- subCadenasBuenas2
          if o(subCadena1 + subCadena2) && ((subCadena1 + subCadena2).length == n)
        } yield subCadena1 + subCadena2

        posiblesCadenas.mkString


      }
  }
  def reconstruirCadenaTurboMejorada (n : Int , o : Oraculo ) : Seq [Char]= {
      def reconstruirCadenaTurboMejoradaAux(acu: Seq[String], n: Int, baseInicial: Int, potencia: Int): Seq[String] = {
        if (baseInicial >= n && acu.nonEmpty) {
          acu
        } else {
          val combinaciones = generarCadenas(n)

          val cadenaSelecta = for {
            cadena <- combinaciones
            if o(cadena)
          } yield cadena

          val acumulado = acu ++ cadenaSelecta
          val base = math.pow(2, potencia).toInt
          reconstruirCadenaTurboMejoradaAux(acumulado, n - base, base, potencia + 1)
        }
      }
      if (n % 2 == 0) {
        val subCadenasCorrectas = reconstruirCadenaTurboMejoradaAux(Seq(), n / 2, 2, 1)

        val posiblesCadenas = for {
          subCadena1 <- subCadenasCorrectas
          subCadena2 <- subCadenasCorrectas
          if o(subCadena1 + subCadena2) && ((subCadena1 + subCadena2).length == n)
        } yield subCadena1 + subCadena2

        posiblesCadenas.mkString
      } else {
        val subCadenasCorrectas1 = reconstruirCadenaTurboMejoradaAux(Seq(), n / 2, 2, 1)
        val subCadenasCorrectas2 = reconstruirCadenaTurboMejoradaAux(Seq(), n - (n / 2), 2, 1)
        val posiblesCadenas = for {
          subCadena1 <- subCadenasCorrectas1
          subCadena2 <- subCadenasCorrectas2
          if o(subCadena1 + subCadena2) && ((subCadena1 + subCadena2).length == n)
        } yield subCadena1 + subCadena2

        posiblesCadenas.mkString
      }
  }
  def reconstruirCadenaTurboAcelerada (n : Int , oraculo : Oraculo ) : Seq [Char]= {
    def reconstruirCadenaTurboAceleradaAux(acu: Seq[String], n:Int,baseInicial:Int,potencia:Int): Seq[String] = {
      if ( baseInicial >= n && acu.length > 0) {
        acu
      } else {
        val combinaciones  = generarCadenas(n)

        val cadenaEncontrada = for {
          cadena <- combinaciones
          if oraculo(cadena)
        } yield cadena

        val acumulado = acu ++ cadenaEncontrada
        val base = math.pow(2,potencia).toInt
        reconstruirCadenaTurboAceleradaAux(acumulado,n-base,base,potencia+1)
      }
    }

    if(n%2 == 0){
      val subCadenasCorrectas = (reconstruirCadenaTurboAceleradaAux(Seq(), n/2,2,1))

      val posiblesCadenas =  for {
        subCadena1 <- subCadenasCorrectas
        subCadena2 <- subCadenasCorrectas
        if  ((subCadena1 + subCadena2).length == n)
      } yield subCadena1 + subCadena2

      val vacio = new Nodo(' ', false, Nil)
      val arbolDePosibilidades = vacio.arbolDeSufijos(posiblesCadenas)
      val posibilidades = vacio.generarPosibilidades(arbolDePosibilidades)
      val cadenaEncontrada = for {
        posibilidad <- posibilidades
        if oraculo(posibilidad)
      } yield posibilidad

      cadenaEncontrada.mkString

    }else{
      val subCadenasCorrectas1 = reconstruirCadenaTurboAceleradaAux(Seq(), n/2,2,1)
      val subCadenasCorrectas2 = reconstruirCadenaTurboAceleradaAux(Seq(), n-(n/2),2,1)
      val posiblesCadenas = for {
        subCadena1 <- subCadenasCorrectas1
        subCadena2 <- subCadenasCorrectas2
        if  ((subCadena1 + subCadena2).length == n)
      } yield subCadena1 + subCadena2

      val vacio = new Nodo(' ', false, Nil)
      val arbolDePosibilidades = vacio.arbolDeSufijos(posiblesCadenas)
      val posibilidades = vacio.generarPosibilidades(arbolDePosibilidades)
      val cadenaEncontrada = for {
        posibilidad <- posibilidades
        if oraculo(posibilidad)
      } yield posibilidad

      cadenaEncontrada.mkString
    }
  }
}


