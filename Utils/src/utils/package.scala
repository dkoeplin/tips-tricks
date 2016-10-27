/**
  * Created by david on 10/26/16.
  */
package object utils {
  def flattenAddress(indices: List[Int], dims: List[Int]) = {
    val strides = dims.scanRight(1){(stride, dim) => stride*dim}.drop(1)
    indices.zip(strides).map{case (i,s) => i*s}.sum
  }

  def ?[T](x: => T): T = x

  type Data = Int
  case class Rep[T](x: T = null.asInstanceOf[T])
  abstract class Def[T] {
    def canBeRewritten: Boolean = true
  }
  def stage[T](x: Def[T]) = Rep[T]()

  def unit[T](x: T) = Rep[T](x)

  object Const {
    def unapply(x: Any): Option[Any] = None
  }
  object Def {
    def unapply(x: Any): Option[Def[_]] = None
  }

  def doRewrite(x: Def[_]) = x

  def doSomething(): Unit = ???
  def doSomethingElse(): Unit = ???
  def doSomethingElseEntirely(): Unit = ???

  trait Something[T]

  def some_scope(x: => Any): Unit = x
}
