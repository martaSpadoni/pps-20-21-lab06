package u06lab.code

/**
  * 1) Implement trait Functions with an object FunctionsImpl such that the code
  * in TryFunctions works correctly.
 */

trait Functions {
  def sum(a: List[Double]): Double
  def concat(a: Seq[String]): String
  def max(a: List[Int]): Int // gives Int.MinValue if a is empty
}

trait Combiner[A] {
  def unit: A
  def combine(a: A, b: A): A
}

object Combiners {

  implicit object SumCombiner extends Combiner[Double] {
    override def unit: Double = 0.0

    override def combine(a: Double, b: Double): Double = a + b
  }

  implicit object ConcatCombiner extends Combiner[String] {
    override def unit: String = ""

    override def combine(a: String, b: String): String = a + b
  }

  implicit object MaxCombiner extends Combiner[Int] {
    override def unit: Int = Int.MinValue

    override def combine(a: Int, b: Int): Int = if (a < b) b else a
  }
}

object FunctionsImpl extends Functions {
  import Combiners._

//  override def sum(a: List[Double]): Double = a.foldRight(0.0)(_+_)
//
//  override def concat(a: Seq[String]): String = a.foldRight("")(_+_)
//
//  override def max(a: List[Int]): Int = a.foldRight(Int.MinValue)((v,acc) => if(v < acc) acc else v)

  override def sum(a: List[Double]): Double = combine(a)

  override def concat(a: Seq[String]): String = combine(a)

  override def max(a: List[Int]): Int = combine(a)

  def combine[A:Combiner](a: Seq[A]): A =
    a.foldRight(implicitly[Combiner[A]].unit)(implicitly[Combiner[A]].combine)
}

object TryFunctions2 extends App {
  val f: Functions = FunctionsImpl
  println(f.sum(List(10.0,20.0,30.1))) // 60.1
  println(f.sum(List()))                // 0.0
  println(f.concat(Seq("a","b","c")))   // abc
  println(f.concat(Seq()))              // ""
  println(f.max(List(-10,3,-5,0)))      // 3
  println(f.max(List()))                // -2147483648
}