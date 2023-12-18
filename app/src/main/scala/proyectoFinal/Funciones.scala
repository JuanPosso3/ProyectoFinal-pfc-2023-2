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
  def generarCadenas(tam: Int): Seq[String] = {
      if (tam== 0) {
        Seq("")
      } else {
        for {
           c <- alfabeto
           combinacion <- generarCadenas(tam- 1)
        } yield c + combinacion
      }
  }
  def reconstruirCadenaIngenuo(n: Int, o: Oraculo): Seq[Char] = {
    val combinaciones = generarCadenas(n)
    combinaciones.find(cadena => o(cadena)).get.toSeq
  }
  def reconstruirCadenaMejorado (n : Int , o : Oraculo ): Seq[Char]= {
      def reconstruirCadenaMejoradoAux(cadena: Seq[Char], combinaciones: Seq[String], acu: Seq[String], n:Int): Seq[String] = {
        if (n == 0) {
          acu
        } else {
          val combinaciones  = generarCadenas(n)

          val cadenaE = for { cadena <- combinaciones
            if o(cadena) == true
          } yield cadena

          val acumula = acu ++ cadenaE
          reconstruirCadenaMejoradoAux(combinaciones.head.toSeq, combinaciones.tail, acumula,n-1)
        }
      }

      if(n%2 == 0){
        val subCadenasBuenas = reconstruirCadenaMejoradoAux( Seq(), Seq(), Seq(), n/2)

        val posiblesCadenas =  for {
          subCadena1 <- subCadenasBuenas
          subCadena2 <- subCadenasBuenas
          if (o(subCadena1 + subCadena2) == true) && ((subCadena1 + subCadena2).length == n)
        } yield subCadena1 + subCadena2

        posiblesCadenas.mkString
      }else{
        val subCadenasBuenas1 = reconstruirCadenaMejoradoAux( Seq(), Seq(), Seq(), n/2)
        val subCadenasBuenas2 = reconstruirCadenaMejoradoAux( Seq(), Seq(), Seq(), n-(n/2))
        val posiblesCadenas = for {
          subCadena1 <- subCadenasBuenas1
          subCadena2 <- subCadenasBuenas2
          if (o(subCadena1 + subCadena2) == true) && ((subCadena1 + subCadena2).length == n)
        } yield subCadena1 + subCadena2

        posiblesCadenas.mkString

      }
  }
  def reconstruirCadenaTurbo (n : Int , o : Oraculo ) : Seq [Char]={
      def reconstruirCadenaTurboAux(cadena: Seq[Char], combinaciones: Seq[String], acu: Seq[String], n:Int): Seq[String] = {
        if (((n == 0 || n == 1) && acu.length > 0) || n < 0 ) {
          acu
        } else {
          val combinaciones  = generarCadenas(n)

          val cadenaE = for {
            cadena <- combinaciones
            if o(cadena) == true
          } yield cadena

          val acumula = acu ++ cadenaE
          reconstruirCadenaTurboAux(combinaciones.head.toSeq, combinaciones.tail, acumula,n-2)
        }
      }
      if(n%2 == 0){
        val subCadenasBuenas= reconstruirCadenaTurboAux( Seq(), Seq(), Seq(), n/2)

        val posiblesCadenas =  for {
          subCadena1 <- subCadenasBuenas
          subCadena2 <- subCadenasBuenas
          if (o(subCadena1 + subCadena2) == true) && ((subCadena1 + subCadena2).length == n)
        } yield subCadena1 + subCadena2

        posiblesCadenas.mkString
      }else{
        val subCadenasBuenas1 = reconstruirCadenaTurboAux( Seq(), Seq(), Seq(), n/2)
        val subCadenasBuenas2 = reconstruirCadenaTurboAux( Seq(), Seq(), Seq(), n-(n/2))
        val posiblesCadenas = for {
          subCadena1 <- subCadenasBuenas1
          subCadena2 <- subCadenasBuenas2
          if (o(subCadena1 + subCadena2) == true) && ((subCadena1 + subCadena2).length == n)
        } yield subCadena1 + subCadena2

        posiblesCadenas.mkString

      }
  }
  def reconstruirCadenaTurboMejorada (n : Int , o : Oraculo ) : Seq [Char]= {
      def reconstruirCadenaTurboMejoradaAux(cadena: Seq[Char], combinaciones: Seq[String], acu: Seq[String], n: Int, baseInicial: Int, potencia: Int): Seq[String] = {
        if (baseInicial >= n && acu.length > 0) {
          acu
        } else {
          val combinaciones = generarCadenas(n)

          val cadenaE = for {
            cadena <- combinaciones
            if o(cadena) == true
          } yield cadena

          val acumula = acu ++ cadenaE
          val base = math.pow(2, potencia).toInt
          reconstruirCadenaTurboMejoradaAux(combinaciones.head.toSeq, combinaciones.tail, acumula, n - base, base, potencia + 1)
        }
      }
      if (n % 2 == 0) {
        val subCadenasCorrectas = reconstruirCadenaTurboMejoradaAux(Seq(), Seq(), Seq(), n / 2, 2, 1)

        val posiblesCadenas = for {
          subCadena1 <- subCadenasCorrectas
          subCadena2 <- subCadenasCorrectas
          if (o(subCadena1 + subCadena2) == true) && ((subCadena1 + subCadena2).length == n)
        } yield subCadena1 + subCadena2

        posiblesCadenas.mkString
      } else {
        val subCadenasCorrectas1 = reconstruirCadenaTurboMejoradaAux(Seq(), Seq(), Seq(), n / 2, 2, 1)
        val subCadenasCorrectas2 = reconstruirCadenaTurboMejoradaAux(Seq(), Seq(), Seq(), n - (n / 2), 2, 1)
        val posiblesCadenas = for {
          subCadena1 <- subCadenasCorrectas1
          subCadena2 <- subCadenasCorrectas2
          if (o(subCadena1 + subCadena2) == true) && ((subCadena1 + subCadena2).length == n)
        } yield subCadena1 + subCadena2

        posiblesCadenas.mkString
      }
  }
  def reconstruirCadenaTurboAcelerada (n : Int , oraculo : Oraculo ) : Seq [Char]= {
    def reconstruirCadenaTurboAceleradaAux(cadena: Seq[Char], combinaciones: Seq[String], acumulador: Seq[String], n:Int,baseInicial:Int,potencia:Int): Seq[String] = {
      if ( baseInicial >= n && acumulador.length > 0) {
        acumulador
      } else {
        val combinaciones  = generarCadenas(n)

        val cadenaEncontrada = for {
          cadena <- combinaciones
          if oraculo(cadena) == true
        } yield cadena

        val acumulacion = acumulador ++ cadenaEncontrada
        val base = math.pow(2,potencia).toInt
        reconstruirCadenaTurboAceleradaAux(combinaciones.head.toSeq, combinaciones.tail, acumulacion,n-base,base,potencia+1)
      }
    }

    if(n%2 == 0){
      val subCadenasCorrectas = (reconstruirCadenaTurboAceleradaAux( Seq(), Seq(), Seq(), n/2,2,1))

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
        if oraculo(posibilidad) == true
      } yield posibilidad

      cadenaEncontrada.mkString

    }else{
      val subCadenasCorrectas1 = reconstruirCadenaTurboAceleradaAux( Seq(), Seq(), Seq(), n/2,2,1)
      val subCadenasCorrectas2 = reconstruirCadenaTurboAceleradaAux( Seq(), Seq(), Seq(), n-(n/2),2,1)
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
        if oraculo(posibilidad) == true
      } yield posibilidad

      cadenaEncontrada.mkString
    }
  }
}


