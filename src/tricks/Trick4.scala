package tricks
import utils._

import scala.annotation.implicitNotFound


object Trick4 {
  /** Type classes **/

  def function[T:Something](t: T) = {

  }

  ?{
    // Equivalent to:
    def function[T](t: T)(implicit evidence: Something[T]) = ???
  }



  ?{
    /** Type classes **/
    trait Something[T] {
      def op1(x: T): T
      def op2(x: T, y: T): T
    }

    val x: Data = ???

    implicit object DataIsSomething extends Something[Data] {
      override def op1(x: Data): Data = x
      override def op2(x: Data, y: Data): Data = x + y
    }

    implicit val local_Evidence_Of_Data_Being_Something: Something[Data] = ???

    implicit def evidence_creator: Something[Data] = ???


    function(x)
  }


  ?{
    trait Something[T] {

    }

    trait Specific[T] extends Something[T] {

    }
  }


  ?{
    /** Practical example **/

    def test[T:Numeric](x: T, y: T): T = implicitly[Numeric[T]].plus(x, y)


    val x: Double = test(1.0, 5.0)

    val y: Int = test(1, 2)

    val z: Float = test(3.0f, 4.0f)
  }


  ?{
    import scala.language.implicitConversions


    @implicitNotFound(msg = s"Type $T does not appear to be a staged type")
    trait StagedType[T] {

    }
  }
}
