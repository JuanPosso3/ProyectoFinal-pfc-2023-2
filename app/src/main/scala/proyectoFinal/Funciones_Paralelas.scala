package proyectoFinal
import common._
import Funciones._
import scala.collection.parallel.CollectionConverters._
object Funciones_Paralelas {

  def reconstruirCadenaIngenuoPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {

    val combinaciones = generarCadenas(n)
    val combinacionesFiltradas = combinaciones.par.filter(o(_) == true)
    val combinacion = combinacionesFiltradas.head
    val cadenaEncontrada = combinacion.toSeq
    cadenaEncontrada
  }
  def reconstruirCadenaMejoradoPar(umbral: Int) (n : Int , o : Oraculo ): Seq[Char]= {
    def reconstruirCadenaMejoradoParallelAux(cadena: Seq[Char], combinaciones: Seq[String], acumulador: Seq[String], n:Int): Seq[String] = {
      if (n == 0) {
        acumulador
      } else {
        val combinaciones  = generarCadenas(n)

        val cadenaEncontrada = for {
          cadena <- combinaciones
          if o(cadena) == true
        } yield cadena

        val acumulacion = acumulador ++ cadenaEncontrada
        reconstruirCadenaMejoradoParallelAux(combinaciones.head.toSeq, combinaciones.tail, acumulacion,n-1)
      }
    }

    if(n%2 == 0){
      val subCadenasCorrectas = task(reconstruirCadenaMejoradoParallelAux( Seq(), Seq(), Seq(), n/2))

      val posiblesCombinaciones =  for {
        subCadena1 <- subCadenasCorrectas.join()
        subCadena2 <- subCadenasCorrectas.join()
        if (o(subCadena1 + subCadena2) == true) && ((subCadena1 + subCadena2).length == n)
      } yield subCadena1 + subCadena2

      posiblesCombinaciones.mkString
    }else{
      val (subCadenasCorrectas1, subCadenasCorrectas2) = parallel(reconstruirCadenaMejoradoParallelAux( Seq(), Seq(), Seq(), n/2),
        reconstruirCadenaMejoradoParallelAux( Seq(), Seq(), Seq(), n-(n/2)))
      val posiblesCombinaciones = for {
        subCadena1 <- subCadenasCorrectas1
        subCadena2 <- subCadenasCorrectas2
        if (o(subCadena1 + subCadena2) == true) && ((subCadena1 + subCadena2).length == n)
      } yield subCadena1 + subCadena2

      posiblesCombinaciones.mkString

    }
  }

  def reconstruirCadenaTurboPar(umbral: Int)(n : Int , o : Oraculo ) : Seq [Char]={
    def reconstruirCadenaTurboParallelAux(cadena: Seq[Char], combinaciones: Seq[String], acumulador: Seq[String], n:Int): Seq[String] = {
      if (((n == 0 || n == 1) && acumulador.length > 0) || n < 0 ) {
        acumulador
      } else {
        val combinaciones  = generarCadenas(n)

        val cadenaEncontrada = for {
          cadena <- combinaciones
          if o(cadena) == true
        } yield cadena

        val acumulacion = acumulador ++ cadenaEncontrada
        reconstruirCadenaTurboParallelAux(combinaciones.head.toSeq, combinaciones.tail, acumulacion,n-2)
      }
    }

    if(n%2 == 0){
      val subCadenasCorrectas = task(reconstruirCadenaTurboParallelAux( Seq(), Seq(), Seq(), n/2))

      val posiblesCombinaciones =  for {
        subCadena1 <- subCadenasCorrectas.join()
        subCadena2 <- subCadenasCorrectas.join()
        if (o(subCadena1 + subCadena2) == true) && ((subCadena1 + subCadena2).length == n)
      } yield subCadena1 + subCadena2

      posiblesCombinaciones.mkString
    }else{
      val (subCadenasCorrectas1,subCadenasCorrectas2) = parallel(reconstruirCadenaTurboParallelAux( Seq(), Seq(), Seq(), n/2)
        ,reconstruirCadenaTurboParallelAux( Seq(), Seq(), Seq(), n-(n/2)))
      val posiblesCombinaciones = for {
        subCadena1 <- subCadenasCorrectas1
        subCadena2 <- subCadenasCorrectas2
        if (o(subCadena1 + subCadena2) == true) && ((subCadena1 + subCadena2).length == n)
      } yield subCadena1 + subCadena2

      posiblesCombinaciones.mkString

    }

  }
  def reconstruirCadenaTurboMejoradaPar(umbral: Int)(n : Int , o : Oraculo ) : Seq [Char]= {
    def reconstruirCadenaTurboMejoradaParallelAux(cadena: Seq[Char], combinaciones: Seq[String], acumulador: Seq[String], n:Int,baseInicial:Int,potencia:Int): Seq[String] = {
      if ( baseInicial >= n && acumulador.length > 0) {
        acumulador
      } else {
        val combinaciones  = generarCadenas(n)

        val cadenaEncontrada = for {
          cadena <- combinaciones
          if o(cadena) == true
        } yield cadena

        val acumulacion = acumulador ++ cadenaEncontrada
        val base = math.pow(2,potencia).toInt
        reconstruirCadenaTurboMejoradaParallelAux(combinaciones.head.toSeq, combinaciones.tail, acumulacion,n-base,base,potencia+1)
      }
    }

    if(n%2 == 0){
      val subCadenasCorrectas = task(reconstruirCadenaTurboMejoradaParallelAux( Seq(), Seq(), Seq(), n/2,2,1))

      val posiblesCombinaciones =  for {
        subCadena1 <- subCadenasCorrectas.join()
        subCadena2 <- subCadenasCorrectas.join()
        if (o(subCadena1 + subCadena2) == true) && ((subCadena1 + subCadena2).length == n)
      } yield subCadena1 + subCadena2

      posiblesCombinaciones.mkString
    }else{
      val (subCadenasCorrectas1,subCadenasCorrectas2) = parallel(reconstruirCadenaTurboMejoradaParallelAux( Seq(), Seq(), Seq(), n/2,2,1)
        ,reconstruirCadenaTurboMejoradaParallelAux( Seq(), Seq(), Seq(), n-(n/2),2,1))
      val posiblesCombinaciones = for {
        subCadena1 <- subCadenasCorrectas1
        subCadena2 <- subCadenasCorrectas2
        if (o(subCadena1 + subCadena2) == true) && ((subCadena1 + subCadena2).length == n)
      } yield subCadena1 + subCadena2

      posiblesCombinaciones.mkString

    }

  }
  def reconstruirCadenaTurboAceleradaPar(umbral: Int)(n : Int , o : Oraculo ) : Seq [Char]= {
    def reconstruirCadenaTurboAceleradaParAux(cadena: Seq[Char], combinaciones: Seq[String], acumulador: Seq[String], n:Int,baseInicial:Int,potencia:Int): Seq[String] = {
      if ( baseInicial >= n && acumulador.length > 0) {
        acumulador
      } else {
        val combinaciones  = generarCadenas(n)

        val cadenaEncontrada = for {
          cadena <- combinaciones
          if o(cadena) == true
        } yield cadena

        val acumulacion = acumulador ++ cadenaEncontrada
        val base = math.pow(2,potencia).toInt
        reconstruirCadenaTurboAceleradaParAux(combinaciones.head.toSeq, combinaciones.tail, acumulacion,n-base,base,potencia+1)
      }
    }

    if(n%2 == 0){
      val subCadenasCorrectas = (reconstruirCadenaTurboAceleradaParAux( Seq(), Seq(), Seq(), n/2,2,1))

      val posiblesCombinaciones =  for {
        subCadena1 <- subCadenasCorrectas
        subCadena2 <- subCadenasCorrectas
        if  ((subCadena1 + subCadena2).length == n)
      } yield subCadena1 + subCadena2

      val vacio = new Nodo(' ', false, Nil)
      val arbolDePosibilidades = task(vacio.arbolDeSufijos(posiblesCombinaciones))
      val posibilidades = vacio.generarPosibilidades(arbolDePosibilidades.join())
      val cadenaEncontrada = for {
        posibilidad <- posibilidades
        if o(posibilidad) == true
      } yield posibilidad

      cadenaEncontrada.mkString


    }else{
      val (subCadenasCorrectas1,subCadenasCorrectas2) = parallel(reconstruirCadenaTurboAceleradaParAux( Seq(), Seq(), Seq(), n/2,2,1)
        , reconstruirCadenaTurboAceleradaParAux( Seq(), Seq(), Seq(), n-(n/2),2,1))
      val posiblesCombinaciones = for {
        subCadena1 <- subCadenasCorrectas1
        subCadena2 <- subCadenasCorrectas2
        if  ((subCadena1 + subCadena2).length == n)
      } yield subCadena1 + subCadena2

      val vacio = new Nodo(' ', false, Nil)
      val arbolDePosibilidades = task(vacio.arbolDeSufijos(posiblesCombinaciones))
      val posibilidades = vacio.generarPosibilidades(arbolDePosibilidades.join())
      val cadenaEncontrada = for {
        posibilidad <- posibilidades
        if o(posibilidad) == true
      } yield posibilidad

      cadenaEncontrada.mkString


    }

  }


}
