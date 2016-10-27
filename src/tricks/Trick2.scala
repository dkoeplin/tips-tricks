package tricks
import utils._
import scala.collection.mutable

/** Rewrite rules **/

trait ArithmeticNodes {

  case class Add(a: Rep[Int], b: Rep[Int]) extends Def[Int]
  case class Sub(a: Rep[Int], b: Rep[Int]) extends Def[Int]

}

object Trick2 extends ArithmeticNodes {
  /** Current version **/
  def add(a: Rep[Int], b: Rep[Int]): Rep[Int] = (a,b) match {

    case (Const(a:Int), Const(b:Int)) => unit(a + b)

    /** Other rules... */

    case _ => stage( Add(a,b) )
  }

  /** Adding new rewrite rules **/
  override def add(a: Rep[Int], b: Rep[Int]) = (a,b) match {
    case (x, Def(Sub(y, z))) if x == z => y   // x + y - z ==> y if x == z

    case _ => super.add(a,b)
  }


  ?{
    /** collect **/
    class List[A] {
      def iterator:Iterator[A] = ???

      def collect[B](func:PartialFunction[A, B]) = {
        var out = List[B]()
        val iter = this.iterator
        while (iter.hasNext) {
          val d = iter.next
          if (func.isDefinedAt(d)) out ::= d // prepend
        }
        out.reverse
      }
    }

    ?{
      val x: PartialFunction[Any, Int] = {case x: Int if x < 3 => x}
    }

    ?{re
      val y:PartialFunction[Def[_], Rep[_]] = {case x:Def[_] if x.canBeRewritten => doRewrite(x) }

      ?{
        def rewriteDef(x: Def[_]) = if (y.isDefinedAt(x)) y(x) else x
      }
    }


    ?{
      /** Syntax sugar for our DSL definitions **/
      val rewriteRules = mutable.HashMap[Class[_], List[PartialFunction[Def[_], Rep[_]]]]()


      def rewrite[D:Manifest](func:PartialFunction[Def[_], Rep[_]]) = {
        val key = manifest[D].runtimeClass
        manifest[D].runtimeClass
        val rules = rewriteRules.getOrElse(key, Nil)

        rewriteRules(key) = rules :+ func
      }


      def add(a:Rep[Int], b:Rep[Int]): Rep[Int] = stage(Add(a, b))

      ? {
        rewrite[Add] {
          case Add(Const(a:Int), Const(b:Int)) => unit(a + b)
          case Add(Const(0), y:Rep[Int]) => y
          case Add(x:Rep[Int], Const(0)) => x
        }
      }

      ? {
        rewrite[Add] {
          case Add(x, Def(Sub(y, z))) if x == z => y // x + y - z ==> y if x == z
          case Add(Def(Sub(x, y)), z) if y == z => x // x - y + z ==> x if y == z
        }
      }
    }
  }


  ?{
    val x: List[String] = ???

    /** Instead of: **/
    x.map{y => y match {
      case "option1" => doSomething()
      case "option2" => doSomethingElse()
    }}

    /** Use this! -- Scala lifts partial functions automatically **/
    x.map{
      case "option1" => doSomething()
      case "option2" => doSomethingElse()
    }

  }
}

