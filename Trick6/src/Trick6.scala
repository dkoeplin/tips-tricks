import scala.reflect.ClassTag
import utils._

object Trick6 {
  /** blackbox macros **/

  case class NDArray[T:ClassTag](private val data: Array[T], private val dims: List[Int]) {
    /** TODO: get an element given an N-D address **/
    def apply(indices: Int*) = data(flattenAddress(indices.toList, dims))


    /** TODO: update an element at a given N-D address **/
    //@CurriedUpdate
    //def __update(indices: Int*)(rhs: T) = data(flattenAddress(indices.toList,dims)) = rhs
    //def update(x: Any*)
    /****/
//    def update(i1: Int, rhs: T): Unit = data(flattenAddress(List(i1),dims)) = rhs
//    def update(i1: Int, i2:Int, rhs: T): Unit = data(flattenAddress(List(i1,i2),dims)) = rhs
//    def update(i1: Int, i2: Int, i3: Int, rhs: T): Unit = data(flattenAddress(List(i1,i2,i3),dims)) = rhs
  }

  def main(args: Array[String]) = {
    val data = Array.fill(125){scala.util.Random.nextInt(32)}
    val data2 = Array.fill(40){scala.util.Random.nextInt(32)}

    val x = NDArray(data, List(5, 5, 5))

    val y = NDArray(data2, List(10, 4))

//    ? {
//      x(3, 3, 3) = 1000
//      println("x(3, 3, 3) = " + x(3, 3, 3))
//
//      y(5, 3) = 100
//      println("y(5,3) = " + y(5, 3))
//    }
  }
}
