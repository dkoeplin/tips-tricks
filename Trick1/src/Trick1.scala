import utils._


/** collect **/

object Trick1 {
  abstract class Things {
    val data: Int
  }
  case class Important(data: Data) extends Things {
    override def toString = data.toString
  }
  case class OtherStuff() extends Things {
    val data = 3
    override def toString = "X"
  }


  /** Vital research goal! **/
  def analyze(data: List[Things]): Boolean = {

    def work(input: List[Important]): Boolean = {
      /** VERY COMPLICATED!!! AVERT YOUR INNOCENT EYES!! **/
      input.nonEmpty
    }

    ? {
      def getImportantThings(data: List[Things]): List[Important] = {
        def version1(input: List[Things]) /*: List[Important]*/ = {
          input.filter {
            case x:Important => true
            case _ => false
          }
        }

        def version2(input: List[Things]): List[Important] = {
          input.filter(_.isInstanceOf[Important]).map(_.asInstanceOf[Important])
        }

        def version3(input: List[Things]): List[Important] = {
          input.flatMap { case x:Important => Some(x); case _ => None }
        }

        def version4(input: List[Things]): List[Important] = {
          input.collect { case x:Important => x }
        }

        ? {
          println(version2(data).mkString(", "))
          println(version3(data).mkString(", "))
          println(version4(data).mkString(", "))
          version4(data)
        }
      }

      work(getImportantThings(data))
    }

  }

  def main(args: Array[String]): Unit = {
    val things = List(Important(1), OtherStuff(), Important(2), Important(5), OtherStuff(), OtherStuff())

    println("data: " + things.mkString(", "))
    analyze(things)

    /** collect: ~1.5x over flatMap, ~2x over filter **/
    //timingAnalysis()
    def timingAnalysis(): Unit = {
      def maybeImportant = if (scala.util.Random.nextBoolean()) Important(3) else OtherStuff

      val data10 = List.tabulate(100000){i => List.tabulate(10){i => maybeImportant}}
      val data100 = List.tabulate(10000){i => List.tabulate(100){i => maybeImportant}}
      val data1000 = List.tabulate(1000){i => List.tabulate(1000){i => maybeImportant}}

      val data = List(data10, data100, data1000)



      for (i <- 0 until 3) {
        println(Math.pow(10, i + 1) + ": ")
        time(1000)("dummy"){
          data(i).foreach { list => list.flatMap { case x:Important => Some(x); case _ => None } }
        }

        time(1000)("flatMap") {
          data(i).foreach { list => list.flatMap { case x:Important => Some(x); case _ => None } }
        }
        time(1000)("filter") {
          data(i).foreach { list => list.filter(_.isInstanceOf[Important]).map(_.asInstanceOf[Important]) }
        }
        time(1000)("collect") {
          data(i).foreach { list => list.collect { case x:Important => x } }
        }
      }

      def time(n: Int)(name: String)(blk: => Any) = {
        val time = System.currentTimeMillis()
        for (i <- 0 until n) {
          blk
        }
        val stop = System.currentTimeMillis()
        val len = (stop - time)/1000.0f

        println(s"$name: $len")
      }


    }
  }


}
