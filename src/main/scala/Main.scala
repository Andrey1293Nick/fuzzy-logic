package hidewise.fuzzy_logic

import FuzzyCollection.toFuzzy

object Main  extends App {
  val e = Seq(1.0, 1.1, 0.8)

  def membershipFunction(double: Double): FuzzyValue = FuzzyValue.unsafe(double)
//  val test: FuzzyCollection[Double] = e.toFuzzy(0, 1.4)

  val seq = Seq(1.0, 2.0, 3.0, 4.0, 5.0)
  val fun = membershipFunction
  val fuzzy = seq.toFuzzy(fun)
  println("tes1t")
  fuzzy.foreach{ v =>
    println(v)
  }
  println("tes2t")
}
