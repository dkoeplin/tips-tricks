package tricks
import utils._
import scala.util.control.NoStackTrace


object Trick3 {
  /** NoStackTrace **/

  def function(x: Data) = x match {
    case a:Data => doSomething()
    case b:Data => doSomethingElse()
    case c:Data => doSomethingElseEntirely()

    case _ => throw new Exception("Oh noes!") with NoStackTrace
  }

  trait Construtor[T] {
    def make(...): T
  }

  implicit object AConstructor extends Construtor[A] {
    def make(...): A = new A(...)
  }
  implicit object BConstructor extends Construtor[B] {
    def make(...): B = new B(...)
  }


  abstract class Maker[T:Construct] {
    def apply(...): T = implicitly[Construtor[T]].make(...)
  }

  object BObj extends Maker[B]
  object AObj extends Maker[A]

}
