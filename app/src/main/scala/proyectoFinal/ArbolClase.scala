package proyectoFinal

abstract class ArbolClase {

  def raiz(t: ArbolClase): Char = {
    t match {
      case Nodo(c, _, _) => c
      case Hoja(c, _) => c
    }
  }

  def cabezas(t: ArbolClase): Seq[Char] = {
    t match {
      case Nodo(_, _, lt) => lt.map(t => raiz(t))
      case Hoja(c, _) => Seq[Char](c)
    }
  }

  def arbolDeSufijos(sufijos: Seq[String]): ArbolClase = {
    sufijos.foldLeft(Nodo(' ', false, List.empty[ArbolClase]): ArbolClase) { (trie, sufijo) =>
      adicionar(trie, sufijo)
    }
  }

  def adicionar(t: ArbolClase, sufijo: String): ArbolClase = sufijo.toList match {
    case Nil => t
    case x :: xs =>
      val nuevoHijo = Nodo(x, true, List.empty[ArbolClase])
      t match {
        case Hoja(c, marcada) =>
          Nodo(c, marcada, List(adicionar(nuevoHijo, xs.mkString)))
        case Nodo(c, marcada, hijos) =>
          val hijoExistente = hijos.find(h => raiz(h) == x)
          val nuevosHijos = hijoExistente match {
            case Some(h) =>
              hijos.updated(hijos.indexOf(h), adicionar(h, xs.mkString))
            case None =>
              hijos :+ adicionar(nuevoHijo, xs.mkString)
          }
          Nodo(c, marcada, nuevosHijos)
      }
  }

  def generarPosibilidades(t: ArbolClase): Seq[String] = {
    def reconstruyendoPosibilidades(t: ArbolClase, prefijo: String): Seq[String] = t match {
      case Hoja(_, _) => Seq(prefijo)
      case Nodo(_, _, hijos) =>
        if (hijos.isEmpty) {
          // Nodo sin hijos, devuelve solo el prefijo actual
          Seq(prefijo)
        } else {
          hijos.flatMap(h => reconstruyendoPosibilidades(h, prefijo + raiz(h)))
        }
    }

    reconstruyendoPosibilidades(t, "")
  }

  def pertenece(s: String, t: ArbolClase): Boolean = {
    t match {
      case Nodo(c, m, lt) => {
        if (s.isEmpty) {
          m
        } else {
          val (hijos, resto) = lt.partition(t => raiz(t) == s.head)
          if (hijos.isEmpty) {
            false
          } else {
            pertenece(s.tail, hijos.head)
          }
        }
      }
      case Hoja(c, m) => {
        if (s.isEmpty) {
          m
        } else {
          false
        }
      }
    }
  }


}
case class Hoja (car:Char, marcada:Boolean) extends ArbolClase
case class Nodo (car:Char, marcada:Boolean,hijos :List[ArbolClase]) extends ArbolClase

