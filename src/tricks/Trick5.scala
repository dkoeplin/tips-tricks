package tricks
import utils._

object Trick5 {
  /** self.type **/

  // Let's try this:
  ?{
    abstract class AbstractGuy {
      def doSomethingAndReturnSelf() = { doSomething(); this }
    }

    case class ConcreteGuy() extends AbstractGuy
    ? {
      val x = ConcreteGuy()
      val y:ConcreteGuy = x.doSomethingAndReturnSelf()
    }

  }


  // However, you can do:
  ?{
    abstract class AbstractGuy { self =>
      def doSomethingAndReturnSelf(): self.type = { doSomething(); this }
    }
    case class ConcreteGuy() extends AbstractGuy

    ?{
      val x = ConcreteGuy()
      val y: ConcreteGuy = x.doSomethingAndReturnSelf()
    }
  }


  // You might want to do:
  ?{
    abstract class AbstractGuy { self =>
      def +(that: self.type): self.type
    }

    case class ConcreteGuy() extends AbstractGuy { self =>
      def +(that: self.type): self.type = {
        this
      } // Some implementation
    }

    ? {
      val x = ConcreteGuy()
      val y = ConcreteGuy()

      val z = x + y // Expected x.type, found y.type
    }
  }

  ?{
    abstract class AbstractGuy { self =>
      def +(that: self.type): self.type

      // + is no longer statically type checked :(
      def +(that: AbstractGuy): self.type = {
        if (this.getClass == that.getClass) {
          this + that.asInstanceOf[self.type]
        }
        else throw new Exception("Type mismatch! Oh noes!")
      }
    }

    ? {
      case class ConcreteGuy() extends AbstractGuy[ConcreteGuy]

      val x = ConcreteGuy()
      val y = ConcreteGuy()

      val z = x + y // Works! Yay!
    }


    ? {
      case class OtherGuy() extends AbstractGuy

      val b = OtherGuy()
      val q = x + b // Will throw an exception at runtime
    }
  }


  // Just for fun:
  ?{
    abstract class AbstractGuy{ ☺ =>
      def doSomethingAndReturnSelf(): ☺.type = {
        this
      }
    }
  }


}
