package hidewise.fuzzy_logic

import FuzzyCollection.toFuzzy

object Main  extends App {
  val e = Seq(1.0, 1.1, 0.8)
  val test: FuzzyCollection[Double] = e.toFuzzy(y(0, 1.4))

  println(test(3))
}
