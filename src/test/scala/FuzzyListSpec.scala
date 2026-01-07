package hidewise.fuzzy_logic

import FuzzyCollection.toFuzzy

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.matchers.should.Matchers._

//todo changed naming
class FuzzyListSpec extends AnyFlatSpec with Matchers  {

  it should "length check" in {
    val seq = Seq(1.0, 2.0, 3.0, 4.0, 5.0)
    val fun = FuzzyListSpec.membershipFunction
    val fuzzy = seq.toFuzzy(fun)

    fuzzy.size shouldBe 5
  }
  it should "check eq seq and fuzzy" in {
    val seq = Seq(1.0, 2.0, 3.0, 4.0, 5.0)
    val fun = FuzzyListSpec.membershipFunction
    val fuzzy = seq.toFuzzy(fun)

    fuzzy.toSet.map(_._1) shouldBe seq.toSet
  }
}

object FuzzyListSpec {
  def membershipFunction(double: Double):FuzzyValue = FuzzyValue.unsafe(double)
}
